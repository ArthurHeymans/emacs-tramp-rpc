//! Filesystem watcher for cache invalidation notifications.
//!
//! Uses inotify (Linux) / kqueue (macOS) via the `notify` crate to watch
//! directories for changes. When changes are detected, a debounced
//! notification is sent to the Emacs client so it can invalidate its caches.

use crate::protocol::{Notification, RpcError};
use crate::{msgpack_map, WriterHandle};
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use rmpv::Value;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};
use tokio::io::AsyncWriteExt;
use tokio::sync::mpsc;
use tokio::time::{self, Duration};

use crate::protocol::from_value;

/// Duration to debounce filesystem events before sending a notification.
/// During bulk operations (e.g. git checkout), many events fire in rapid
/// succession. We collect them all and send a single notification.
const DEBOUNCE_DURATION: Duration = Duration::from_millis(200);

/// Global WatchManager instance, initialized in main().
static WATCH_MANAGER: OnceLock<Arc<WatchManager>> = OnceLock::new();

/// Get the global WatchManager, if initialized.
pub fn get() -> Option<&'static Arc<WatchManager>> {
    WATCH_MANAGER.get()
}

/// Initialize the global WatchManager. Called once from main().
pub fn init(manager: Arc<WatchManager>) {
    let _ = WATCH_MANAGER.set(manager);
}

/// Helper to lock a std::sync::Mutex, recovering from poisoning.
/// The data is still valid after a panic, so we just unwrap the poison error.
fn lock_or_recover<T>(mutex: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|e| e.into_inner())
}

/// Manages filesystem watchers and sends change notifications to the client.
pub struct WatchManager {
    /// The underlying OS watcher (inotify/kqueue).
    /// Protected by std::sync::Mutex because notify's callback runs on its
    /// own thread, not a tokio thread.
    watcher: Mutex<RecommendedWatcher>,

    /// Currently watched paths: maps the canonical path used for the watch
    /// to its recursive mode. We store the canonical path from watch() so
    /// that unwatch() doesn't need to re-canonicalize (which would fail if
    /// the directory has been deleted).
    watched_paths: Mutex<HashMap<PathBuf, RecursiveMode>>,
}

impl WatchManager {
    /// Create a new WatchManager and spawn the debounce background task.
    ///
    /// The debounce task receives raw inotify events, batches them over a
    /// short window, and writes `fs.changed` notifications to the client
    /// via the shared stdout writer.
    pub fn new(writer: WriterHandle) -> Result<Arc<Self>, notify::Error> {
        let (tx, rx) = mpsc::unbounded_channel();

        let watcher = RecommendedWatcher::new(
            move |event: notify::Result<Event>| {
                if let Ok(event) = event {
                    // Only forward events that indicate filesystem mutations
                    match event.kind {
                        EventKind::Create(_)
                        | EventKind::Modify(_)
                        | EventKind::Remove(_) => {
                            let _ = tx.send(event);
                        }
                        _ => {} // Ignore Access, Other events
                    }
                }
            },
            Config::default(),
        )?;

        let manager = Arc::new(Self {
            watcher: Mutex::new(watcher),
            watched_paths: Mutex::new(HashMap::new()),
        });

        // Spawn the debounce background task
        tokio::spawn(debounce_loop(rx, writer));

        Ok(manager)
    }

    /// Start watching a path for filesystem changes.
    ///
    /// If `recursive` is true, all subdirectories are also watched.
    /// Returns an error if the path doesn't exist or watch limits are exceeded.
    pub fn watch(&self, path: &Path, recursive: bool) -> Result<(), notify::Error> {
        let mode = if recursive {
            RecursiveMode::Recursive
        } else {
            RecursiveMode::NonRecursive
        };

        let canonical = path.canonicalize().map_err(|e| {
            notify::Error::generic(&format!("Failed to canonicalize {}: {}", path.display(), e))
        })?;

        let mut watcher = lock_or_recover(&self.watcher);
        watcher.watch(&canonical, mode)?;

        let mut paths = lock_or_recover(&self.watched_paths);
        paths.insert(canonical, mode);

        Ok(())
    }

    /// Stop watching a path.
    ///
    /// Looks up the stored canonical path from when watch() was called,
    /// so this works even if the directory has been deleted since then.
    pub fn unwatch(&self, path: &Path) -> Result<(), notify::Error> {
        // Try to canonicalize, but fall back to looking up the raw path
        // in our stored paths (which were canonicalized at watch time).
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

        // Find the matching stored path. First try exact match on canonical,
        // then try matching against the raw input path.
        let stored_path = {
            let paths = lock_or_recover(&self.watched_paths);
            if paths.contains_key(&canonical) {
                Some(canonical.clone())
            } else {
                // Fallback: the directory may have been deleted so canonicalize
                // returned the raw path. Search stored paths for one that ends
                // with the same components.
                paths
                    .keys()
                    .find(|stored| stored.ends_with(path) || path.ends_with(stored.as_path()))
                    .cloned()
            }
        };

        let stored_path = stored_path.ok_or_else(|| {
            notify::Error::generic(&format!("Path not being watched: {}", path.display()))
        })?;

        let mut watcher = lock_or_recover(&self.watcher);
        watcher.unwatch(&stored_path)?;

        let mut paths = lock_or_recover(&self.watched_paths);
        paths.remove(&stored_path);

        Ok(())
    }

    /// List currently watched paths and whether they are recursive.
    pub fn list(&self) -> Vec<(PathBuf, bool)> {
        let paths = lock_or_recover(&self.watched_paths);
        paths
            .iter()
            .map(|(p, m)| (p.clone(), matches!(m, RecursiveMode::Recursive)))
            .collect()
    }
}

/// Background task: receives raw inotify events, debounces them, and sends
/// batched `fs.changed` notifications to the Emacs client.
///
/// Algorithm (fixed-window debounce):
/// 1. Wait for the first event (blocks until something happens)
/// 2. Start a 200ms timer
/// 3. Collect all events that arrive during the timer window
/// 4. When the timer fires, send one notification with all unique paths
/// 5. Go back to step 1
async fn debounce_loop(mut rx: mpsc::UnboundedReceiver<Event>, writer: WriterHandle) {
    loop {
        // Phase 1: Wait for the first event
        let event = match rx.recv().await {
            Some(e) => e,
            None => break, // Channel closed, watcher dropped
        };

        let mut pending_paths: HashSet<PathBuf> = HashSet::new();
        for path in event.paths {
            pending_paths.insert(path);
        }

        // Phase 2: Collect more events during the debounce window
        let deadline = time::Instant::now() + DEBOUNCE_DURATION;
        loop {
            tokio::select! {
                _ = time::sleep_until(deadline) => {
                    break; // Debounce window expired
                }
                event = rx.recv() => {
                    match event {
                        Some(e) => {
                            for path in e.paths {
                                pending_paths.insert(path);
                            }
                        }
                        None => return, // Channel closed
                    }
                }
            }
        }

        // Phase 3: Send notification with all collected paths
        if !pending_paths.is_empty() {
            if let Err(e) = send_notification(&writer, &pending_paths).await {
                eprintln!("Failed to send fs.changed notification: {}", e);
                // Stdout is broken (Emacs disconnected), stop the loop
                break;
            }
        }
    }
}

/// Serialize and send an `fs.changed` notification over the stdout writer.
/// Returns an error if serialization or writing fails.
async fn send_notification(
    writer: &WriterHandle,
    paths: &HashSet<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    let paths_value: Vec<Value> = paths
        .iter()
        .map(|p| Value::String(p.to_string_lossy().to_string().into()))
        .collect();

    let notification = Notification {
        version: "2.0".to_string(),
        method: "fs.changed".to_string(),
        params: Value::Map(vec![(
            Value::String("paths".into()),
            Value::Array(paths_value),
        )]),
    };

    let bytes = rmp_serde::to_vec_named(&notification)?;
    let mut w = writer.lock().await;
    let len_bytes = (bytes.len() as u32).to_be_bytes();
    w.write_all(&len_bytes).await?;
    w.write_all(&bytes).await?;
    w.flush().await?;
    Ok(())
}

// ============================================================================
// RPC handlers for watch.add, watch.remove, watch.list
// ============================================================================

type HandlerResult = Result<Value, RpcError>;

/// Handle `watch.add` - start watching a directory for changes.
///
/// Params: { "path": "/path/to/dir", "recursive": true|false }
pub fn handle_add(params: &Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
        #[serde(default = "default_recursive")]
        recursive: bool,
    }
    fn default_recursive() -> bool {
        true
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    manager
        .watch(Path::new(&params.path), params.recursive)
        .map_err(|e| RpcError::internal_error(format!("Failed to watch: {}", e)))?;

    Ok(msgpack_map! {
        "path" => params.path.clone(),
        "recursive" => Value::Boolean(params.recursive)
    })
}

/// Handle `watch.remove` - stop watching a directory.
///
/// Params: { "path": "/path/to/dir" }
pub fn handle_remove(params: &Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    manager
        .unwatch(Path::new(&params.path))
        .map_err(|e| RpcError::internal_error(format!("Failed to unwatch: {}", e)))?;

    Ok(Value::Boolean(true))
}

/// Handle `watch.list` - list currently watched paths.
///
/// Params: {} (none)
pub fn handle_list(_params: &Value) -> HandlerResult {
    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    let watches: Vec<Value> = manager
        .list()
        .into_iter()
        .map(|(path, recursive)| {
            msgpack_map! {
                "path" => path.to_string_lossy().to_string(),
                "recursive" => Value::Boolean(recursive)
            }
        })
        .collect();

    Ok(Value::Array(watches))
}
