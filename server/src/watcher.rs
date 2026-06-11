//! Filesystem watcher for cache invalidation notifications.
//!
//! Uses inotify (Linux) / kqueue (macOS) via the `notify` crate to watch
//! directories for changes. When changes are detected, a debounced
//! notification is sent to the Emacs client so it can invalidate its caches.

use crate::protocol::{Notification, RpcError};
use crate::{msgpack_map, WriterHandle};
use notify::event::{ModifyKind, RemoveKind};
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use rmpv::Value;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock, Weak};
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

/// .gitignore-aware wrapper around `RecommendedWatcher`.
///
/// Recursive watches are registered as per-directory non-recursive watches.
struct FilteredWatcher {
    inner: RecommendedWatcher,
    recursive_roots: HashMap<PathBuf, HashSet<PathBuf>>,
    direct_watches: HashSet<PathBuf>,
    path_watch_counts: HashMap<PathBuf, usize>,
}

impl FilteredWatcher {
    fn new<F>(handler: F) -> Result<Self, notify::Error>
    where
        F: notify::EventHandler,
    {
        Ok(Self {
            inner: RecommendedWatcher::new(handler, Config::default())?,
            recursive_roots: HashMap::new(),
            direct_watches: HashSet::new(),
            path_watch_counts: HashMap::new(),
        })
    }

    fn watch(&mut self, path: &Path, mode: RecursiveMode) -> Result<(), notify::Error> {
        match mode {
            RecursiveMode::NonRecursive => self.watch_nonrecursive(path),
            RecursiveMode::Recursive => self.watch_recursive(path),
        }
    }

    fn unwatch(&mut self, path: &Path) -> Result<(), notify::Error> {
        if let Some(dirs) = self.recursive_roots.remove(path) {
            for p in &dirs {
                self.remove_path_watch_best_effort(p);
            }
            Ok(())
        } else if self.direct_watches.contains(path) {
            self.remove_path_watch(path)?;
            self.direct_watches.remove(path);
            Ok(())
        } else {
            self.inner.unwatch(path)
        }
    }

    fn watch_nonrecursive(&mut self, path: &Path) -> Result<(), notify::Error> {
        let path = path.to_path_buf();
        if !self.direct_watches.insert(path.clone()) {
            return Ok(());
        }

        if let Err(err) = self.add_path_watch(&path) {
            self.direct_watches.remove(&path);
            return Err(err);
        }
        Ok(())
    }

    fn watch_recursive(&mut self, path: &Path) -> Result<(), notify::Error> {
        let dirs = Self::collect_recursive_dirs(path);
        if self.recursive_roots.contains_key(path) {
            return self.apply_recursive_dirs(path, dirs);
        }

        // Seed the root with an empty set so initial registration can use the
        // same diff-and-rollback path as later refreshes.
        self.recursive_roots
            .insert(path.to_path_buf(), HashSet::new());
        if let Err(err) = self.apply_recursive_dirs(path, dirs) {
            self.recursive_roots.remove(path);
            return Err(err);
        }

        Ok(())
    }

    fn recursive_roots_for_paths(&self, paths: &[PathBuf]) -> Vec<PathBuf> {
        self.recursive_roots
            .iter()
            .filter(|(root, dirs)| {
                paths
                    .iter()
                    .any(|path| path.starts_with(root) && (path.is_dir() || dirs.contains(path)))
            })
            .map(|(root, _)| root.clone())
            .collect()
    }

    /// Recursive roots affected by an observed Git ignore-file change.
    fn recursive_roots_for_ignore_rule(&self, path: &Path) -> Vec<PathBuf> {
        let Some(scope) = ignore_rule_scope(path) else {
            return Vec::new();
        };

        self.recursive_roots
            .keys()
            .filter(|root| root.starts_with(&scope) || scope.starts_with(root))
            .cloned()
            .collect()
    }

    /// Reconcile one recursive root to a freshly scanned directory set.
    fn apply_recursive_dirs(
        &mut self,
        root: &Path,
        next: HashSet<PathBuf>,
    ) -> Result<(), notify::Error> {
        let Some(current) = self.recursive_roots.get(root).cloned() else {
            return Ok(());
        };

        let to_remove: Vec<_> = current.difference(&next).cloned().collect();
        let to_add: Vec<_> = next.difference(&current).cloned().collect();
        let mut applied = current;
        let mut added: Vec<PathBuf> = Vec::new();

        // Remove first so rename old -> new cannot reuse old's descriptor.
        for dir in &to_remove {
            self.remove_path_watch_best_effort(dir);
            applied.remove(dir);
        }

        for dir in &to_add {
            if let Err(err) = self.add_path_watch(dir) {
                for added_dir in &added {
                    self.remove_path_watch_best_effort(added_dir);
                    applied.remove(added_dir);
                }
                self.recursive_roots.insert(root.to_path_buf(), applied);
                return Err(err);
            }
            added.push(dir.clone());
            applied.insert(dir.clone());
        }

        self.recursive_roots.insert(root.to_path_buf(), next);
        Ok(())
    }

    fn add_path_watch(&mut self, path: &Path) -> Result<(), notify::Error> {
        // The logical refcount can outlive the backend watch after inode replacement.
        self.inner.watch(path, RecursiveMode::NonRecursive)?;
        *self
            .path_watch_counts
            .entry(path.to_path_buf())
            .or_insert(0) += 1;
        Ok(())
    }

    /// Rebind an existing backend watch without changing logical ownership.
    fn rearm_existing_watch(&mut self, path: &Path) -> Result<(), notify::Error> {
        if !self.path_watch_counts.contains_key(path) {
            return Ok(());
        }
        let _ = self.inner.unwatch(path);
        self.inner.watch(path, RecursiveMode::NonRecursive)
    }

    fn watched_paths_under(&self, roots: &[PathBuf]) -> Vec<PathBuf> {
        let mut paths: Vec<_> = self
            .path_watch_counts
            .keys()
            .filter(|path| roots.iter().any(|root| path.starts_with(root)))
            .cloned()
            .collect();
        paths.sort();
        paths.dedup();
        paths
    }

    fn remove_path_watch(&mut self, path: &Path) -> Result<(), notify::Error> {
        match self.path_watch_counts.get(path).copied() {
            Some(count) if count > 1 => {
                if let Some(count) = self.path_watch_counts.get_mut(path) {
                    *count -= 1;
                }
                Ok(())
            }
            Some(_) => {
                self.inner.unwatch(path)?;
                self.path_watch_counts.remove(path);
                Ok(())
            }
            None => self.inner.unwatch(path),
        }
    }

    /// Teardown variant: drop logical ownership even if backend unwatch fails.
    fn remove_path_watch_best_effort(&mut self, path: &Path) {
        match self.path_watch_counts.get(path).copied() {
            Some(count) if count > 1 => {
                if let Some(count) = self.path_watch_counts.get_mut(path) {
                    *count -= 1;
                }
            }
            Some(_) => {
                let _ = self.inner.unwatch(path);
                self.path_watch_counts.remove(path);
            }
            None => {
                let _ = self.inner.unwatch(path);
            }
        }
    }

    fn collect_recursive_dirs(root: &Path) -> HashSet<PathBuf> {
        // Git-aware only: ignore Git sources, not generic `.ignore` files.
        // hidden(false): include .git/, which Magit cares about.
        let walker = ignore::WalkBuilder::new(root)
            .standard_filters(true)
            .ignore(false)
            .hidden(false)
            // Match notify's recursive watcher behavior: recursive watches
            // follow symlinked directories and install watches below them.
            .follow_links(true)
            .build();

        let mut dirs = HashSet::new();
        for entry in walker {
            let entry = match entry {
                Ok(e) => e,
                Err(_) => continue, // skip unreadable paths, keep walking
            };
            if entry.file_type().is_some_and(|ft| ft.is_dir()) {
                dirs.insert(entry.path().to_path_buf());
            }
        }
        dirs
    }
}

/// Manages filesystem watchers and sends change notifications to the client.
pub struct WatchManager {
    /// The underlying OS watcher (inotify/kqueue).
    /// Protected by std::sync::Mutex because notify's callback runs on its
    /// own thread, not a tokio thread.
    watcher: Mutex<FilteredWatcher>,

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

        let watcher = FilteredWatcher::new(move |event: notify::Result<Event>| {
            if let Ok(event) = event {
                // Only forward events that indicate filesystem mutations
                match event.kind {
                    EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_) => {
                        // Topology events cannot be dropped, and blocking here can
                        // deadlock notify's watch/unwatch event loop.
                        let _ = tx.send(event);
                    }
                    _ => {} // Ignore Access, Other events
                }
            }
        })?;

        let manager = Arc::new(Self {
            watcher: Mutex::new(watcher),
            watched_paths: Mutex::new(HashMap::new()),
        });

        // Spawn the debounce background task
        tokio::spawn(debounce_loop(rx, writer, Arc::downgrade(&manager)));

        Ok(manager)
    }

    /// Start watching a path for filesystem changes.
    ///
    /// If `recursive` is true, all subdirectories are also watched.
    /// Returns an error if the path doesn't exist or watch limits are exceeded.
    ///
    /// Repeated watches are idempotent; non-recursive watches can be upgraded.
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
        let mut paths = lock_or_recover(&self.watched_paths);

        match paths.get(&canonical).copied() {
            Some(existing) if existing == mode => return Ok(()),
            Some(RecursiveMode::Recursive) => return Ok(()),
            Some(RecursiveMode::NonRecursive) => {
                watcher.unwatch(&canonical)?;
                if let Err(err) = watcher.watch(&canonical, RecursiveMode::Recursive) {
                    if watcher
                        .watch(&canonical, RecursiveMode::NonRecursive)
                        .is_err()
                    {
                        paths.remove(&canonical);
                    }
                    return Err(err);
                }
                paths.insert(canonical, RecursiveMode::Recursive);
            }
            None => {
                watcher.watch(&canonical, mode)?;
                paths.insert(canonical, mode);
            }
        }

        Ok(())
    }

    /// Stop watching a path.
    ///
    /// Looks up the stored canonical path from when watch() was called,
    /// so this works even if the directory has been deleted since then.
    ///
    /// Lock ordering: watcher -> watched_paths (same as watch()).
    pub fn unwatch(&self, path: &Path) -> Result<(), notify::Error> {
        // Try to canonicalize, but fall back to the raw path
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

        // Acquire locks in consistent order: watcher first, then watched_paths
        let mut watcher = lock_or_recover(&self.watcher);
        let mut paths = lock_or_recover(&self.watched_paths);

        // Find the matching stored path using exact canonical path matching only.
        if !paths.contains_key(&canonical) {
            return Err(notify::Error::generic(&format!(
                "Path not being watched (canonical: {}): {}",
                canonical.display(),
                path.display()
            )));
        }

        watcher.unwatch(&canonical)?;
        paths.remove(&canonical);

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

    fn recursive_roots_for_event(&self, event: &Event) -> HashSet<PathBuf> {
        let refresh_paths = directory_tree_refresh_paths(event);
        let ignore_paths = ignore_rule_paths(event);
        if refresh_paths.is_empty() && ignore_paths.is_empty() {
            return HashSet::new();
        }

        let watcher = lock_or_recover(&self.watcher);
        let mut roots_to_refresh: HashSet<_> = watcher
            .recursive_roots_for_paths(&refresh_paths)
            .into_iter()
            .collect();

        for path in ignore_paths {
            roots_to_refresh.extend(watcher.recursive_roots_for_ignore_rule(&path));
        }

        roots_to_refresh
    }

    fn refresh_recursive_roots(&self, roots_to_refresh: HashSet<PathBuf>) {
        if roots_to_refresh.is_empty() {
            return;
        }

        let refreshed_roots: Vec<_> = roots_to_refresh
            .into_iter()
            .map(|root| {
                let dirs = FilteredWatcher::collect_recursive_dirs(&root);
                (root, dirs)
            })
            .collect();

        let mut watcher = lock_or_recover(&self.watcher);
        for (root, dirs) in refreshed_roots {
            let _ = watcher.apply_recursive_dirs(&root, dirs);
        }
    }

    /// Rebind still-existing watches after Linux/inotify inode replacement.
    ///
    /// Must run after `refresh_recursive_roots`, so genuine deletes are already
    /// pruned and only path-identical replacements remain.
    fn rearm_suspect_paths(&self, suspect_paths: HashSet<PathBuf>) {
        let existing_roots: Vec<PathBuf> = suspect_paths
            .into_iter()
            .filter(|path| path.is_dir())
            .collect();
        if existing_roots.is_empty() {
            return;
        }

        let candidates = {
            let watcher = lock_or_recover(&self.watcher);
            watcher.watched_paths_under(&existing_roots)
        };
        let existing: Vec<PathBuf> = candidates
            .into_iter()
            .filter(|path| path.is_dir())
            .collect();
        if existing.is_empty() {
            return;
        }

        let mut watcher = lock_or_recover(&self.watcher);
        for path in &existing {
            // Best effort: keep logical ownership even if the backend refuses.
            let _ = watcher.rearm_existing_watch(path);
        }
    }
}

fn directory_tree_refresh_paths(event: &Event) -> Vec<PathBuf> {
    match event.kind {
        EventKind::Create(_) | EventKind::Modify(ModifyKind::Any | ModifyKind::Other) => {
            existing_directory_paths(&event.paths)
        }
        EventKind::Modify(ModifyKind::Name(_)) => event.paths.clone(),
        EventKind::Remove(RemoveKind::Any | RemoveKind::Folder | RemoveKind::Other) => {
            event.paths.clone()
        }
        _ => Vec::new(),
    }
}

fn existing_directory_paths(paths: &[PathBuf]) -> Vec<PathBuf> {
    paths.iter().filter(|path| path.is_dir()).cloned().collect()
}

/// Linux/inotify inputs for [`WatchManager::rearm_suspect_paths`].
#[cfg(target_os = "linux")]
fn inode_replacing_paths(event: &Event) -> Vec<PathBuf> {
    match event.kind {
        EventKind::Remove(_) | EventKind::Modify(ModifyKind::Name(_)) => event.paths.clone(),
        _ => Vec::new(),
    }
}

#[cfg(not(target_os = "linux"))]
fn inode_replacing_paths(_event: &Event) -> Vec<PathBuf> {
    Vec::new()
}

fn ignore_rule_paths(event: &Event) -> Vec<PathBuf> {
    if !matches!(
        event.kind,
        EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_)
    ) {
        return Vec::new();
    }

    event
        .paths
        .iter()
        .filter(|path| is_ignore_rule_path(path))
        .cloned()
        .collect()
}

fn is_ignore_rule_path(path: &Path) -> bool {
    if path.file_name().is_some_and(|name| name == ".gitignore") {
        return true;
    }

    path.file_name().is_some_and(|name| name == "exclude")
        && path
            .parent()
            .and_then(Path::file_name)
            .is_some_and(|name| name == "info")
        && path
            .parent()
            .and_then(Path::parent)
            .and_then(Path::file_name)
            .is_some_and(|name| name == ".git")
}

fn ignore_rule_scope(path: &Path) -> Option<PathBuf> {
    if path.file_name().is_some_and(|name| name == ".gitignore") {
        return path.parent().map(Path::to_path_buf);
    }

    if is_ignore_rule_path(path) {
        return path
            .parent()
            .and_then(Path::parent)
            .and_then(Path::parent)
            .map(Path::to_path_buf);
    }

    None
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
async fn debounce_loop(
    mut rx: mpsc::UnboundedReceiver<Event>,
    writer: WriterHandle,
    manager: Weak<WatchManager>,
) {
    loop {
        // Phase 1: Wait for the first event
        let event = match rx.recv().await {
            Some(e) => e,
            None => break, // Channel closed, watcher dropped
        };

        let mut pending_paths: HashSet<PathBuf> = HashSet::new();
        let mut roots_to_refresh: HashSet<PathBuf> = HashSet::new();
        let mut suspect_paths: HashSet<PathBuf> = HashSet::new();
        collect_event(
            event,
            &manager,
            &mut pending_paths,
            &mut roots_to_refresh,
            &mut suspect_paths,
        );

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
                            collect_event(
                                e,
                                &manager,
                                &mut pending_paths,
                                &mut roots_to_refresh,
                                &mut suspect_paths,
                            );
                        }
                        None => return, // Channel closed
                    }
                }
            }
        }

        if let Some(manager) = manager.upgrade() {
            manager.refresh_recursive_roots(roots_to_refresh);
            manager.rearm_suspect_paths(suspect_paths);
        }

        // Phase 3: Send notification with all collected paths
        if !pending_paths.is_empty() && send_notification(&writer, &pending_paths).await.is_err() {
            // Stdout is broken (Emacs disconnected), stop the loop.
            // Cannot use eprintln! as SSH merges stderr with stdout.
            break;
        }
    }
}

fn collect_event(
    event: Event,
    manager: &Weak<WatchManager>,
    pending_paths: &mut HashSet<PathBuf>,
    roots_to_refresh: &mut HashSet<PathBuf>,
    suspect_paths: &mut HashSet<PathBuf>,
) {
    if let Some(manager) = manager.upgrade() {
        roots_to_refresh.extend(manager.recursive_roots_for_event(&event));
    }

    suspect_paths.extend(inode_replacing_paths(&event));
    pending_paths.extend(event.paths);
}

/// Serialize and send an `fs.changed` notification over the stdout writer.
/// Returns an error if serialization or writing fails.
async fn send_notification(
    writer: &WriterHandle,
    paths: &HashSet<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    let paths_value: Vec<Value> = paths
        .iter()
        .map(|p| Value::String(p.to_string_lossy().into_owned().into()))
        .collect();

    let notification = Notification::new(
        "fs.changed",
        Value::Map(vec![(
            Value::String("paths".into()),
            Value::Array(paths_value),
        )]),
    );

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

use crate::handlers::HandlerResult;

/// Handle `watch.add` - start watching a directory for changes.
///
/// Params: { "path": "/path/to/dir", "recursive": true|false }
pub fn handle_add(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
        #[serde(default = "default_recursive")]
        recursive: bool,
    }
    fn default_recursive() -> bool {
        true
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let expanded = crate::handlers::expand_tilde(&params.path);

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    manager
        .watch(Path::new(&expanded), params.recursive)
        .map_err(|e| RpcError::internal_error(format!("Failed to watch: {}", e)))?;

    Ok(msgpack_map! {
        "path" => expanded.clone(),
        "recursive" => Value::Boolean(params.recursive)
    })
}

/// Handle `watch.remove` - stop watching a directory.
///
/// Params: { "path": "/path/to/dir" }
pub fn handle_remove(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let expanded = crate::handlers::expand_tilde(&params.path);

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    manager
        .unwatch(Path::new(&expanded))
        .map_err(|e| RpcError::internal_error(format!("Failed to unwatch: {}", e)))?;

    Ok(Value::Boolean(true))
}

/// Handle `watch.list` - list currently watched paths.
///
/// Params: {} (none)
pub fn handle_list(_params: Value) -> HandlerResult {
    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    let watches: Vec<Value> = manager
        .list()
        .into_iter()
        .map(|(path, recursive)| {
            msgpack_map! {
                "path" => path.to_string_lossy().into_owned(),
                "recursive" => Value::Boolean(recursive)
            }
        })
        .collect();

    Ok(Value::Array(watches))
}

#[cfg(test)]
mod tests {
    use super::*;
    use notify::event::{CreateKind, RenameMode};
    use std::fs;
    #[cfg(target_os = "linux")]
    use std::sync::mpsc as std_mpsc;
    #[cfg(target_os = "linux")]
    use std::time::{Duration, Instant};

    fn test_manager() -> WatchManager {
        WatchManager {
            watcher: Mutex::new(FilteredWatcher::new(|_: notify::Result<Event>| {}).unwrap()),
            watched_paths: Mutex::new(HashMap::new()),
        }
    }

    fn refresh_for_event(manager: &WatchManager, event: &Event) {
        let roots = manager.recursive_roots_for_event(event);
        manager.refresh_recursive_roots(roots);
    }

    #[cfg(target_os = "linux")]
    fn drain_events(rx: &std_mpsc::Receiver<Event>) {
        while rx.try_recv().is_ok() {}
    }

    #[cfg(target_os = "linux")]
    fn recv_event_matching<F>(rx: &std_mpsc::Receiver<Event>, timeout: Duration, mut matches: F)
    where
        F: FnMut(&Event) -> bool,
    {
        let deadline = Instant::now() + timeout;
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                panic!("timed out waiting for matching notify event");
            }

            match rx.recv_timeout(remaining) {
                Ok(event) if matches(&event) => return,
                Ok(_) => {}
                Err(err) => panic!("timed out waiting for notify event: {err}"),
            }
        }
    }

    #[test]
    fn test_recursive_scan_skips_gitignored_directories() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        fs::create_dir(root.join(".git")).unwrap();
        fs::write(root.join(".gitignore"), "ignored/\n").unwrap();
        fs::create_dir_all(root.join("ignored/nested")).unwrap();
        fs::create_dir_all(root.join("src")).unwrap();

        let dirs = FilteredWatcher::collect_recursive_dirs(&root);

        assert!(dirs.contains(&root));
        assert!(dirs.contains(&root.join("src")));
        assert!(!dirs.contains(&root.join("ignored")));
        assert!(!dirs.contains(&root.join("ignored/nested")));
    }

    #[test]
    fn test_recursive_scan_honors_parent_gitignore() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let sub = root.join("sub");
        fs::create_dir(root.join(".git")).unwrap();
        fs::write(root.join(".gitignore"), "sub/ignored/\n").unwrap();
        fs::create_dir_all(sub.join("ignored")).unwrap();
        fs::create_dir_all(sub.join("tracked")).unwrap();

        let dirs = FilteredWatcher::collect_recursive_dirs(&sub);

        assert!(dirs.contains(&sub));
        assert!(dirs.contains(&sub.join("tracked")));
        assert!(!dirs.contains(&sub.join("ignored")));
    }

    #[test]
    fn test_recursive_scan_ignores_dot_ignore_files() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        fs::create_dir(root.join(".git")).unwrap();
        fs::write(root.join(".ignore"), "build/\n").unwrap();
        fs::write(root.join(".gitignore"), "gitignored/\n").unwrap();
        fs::create_dir_all(root.join("build")).unwrap();
        fs::create_dir_all(root.join("gitignored")).unwrap();

        let dirs = FilteredWatcher::collect_recursive_dirs(&root);

        assert!(dirs.contains(&root.join("build")));
        assert!(!dirs.contains(&root.join("gitignored")));
    }

    #[cfg(unix)]
    #[test]
    fn test_recursive_scan_follows_symlinked_directories() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let real = root.join("real");
        let link = root.join("link");
        fs::create_dir_all(real.join("nested")).unwrap();
        std::os::unix::fs::symlink(&real, &link).unwrap();

        let dirs = FilteredWatcher::collect_recursive_dirs(&root);

        assert!(dirs.contains(&root));
        assert!(dirs.contains(&real));
        assert!(dirs.contains(&real.join("nested")));
        assert!(dirs.contains(&link));
        assert!(dirs.contains(&link.join("nested")));
    }

    #[test]
    fn test_refresh_adds_new_directory_under_recursive_root() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let src = root.join("src");
        let new_dir = src.join("new");
        fs::create_dir_all(&src).unwrap();

        let manager = test_manager();
        manager.watch(&root, true).unwrap();

        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(!watcher.recursive_roots[&root].contains(&new_dir));
        }

        fs::create_dir_all(&new_dir).unwrap();
        let event = Event::new(EventKind::Create(CreateKind::Folder)).add_path(new_dir.clone());
        refresh_for_event(&manager, &event);

        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots[&root].contains(&new_dir));
            assert_eq!(watcher.path_watch_counts.get(&new_dir), Some(&1));
        }
        manager.unwatch(&root).unwrap();
    }

    #[test]
    fn test_refresh_does_not_watch_new_gitignored_directory() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        fs::create_dir(root.join(".git")).unwrap();
        fs::write(root.join(".gitignore"), "ignored/\n").unwrap();

        let ignored_dir = root.join("ignored");
        let manager = test_manager();
        manager.watch(&root, true).unwrap();

        fs::create_dir_all(&ignored_dir).unwrap();
        let event = Event::new(EventKind::Create(CreateKind::Folder)).add_path(ignored_dir.clone());
        refresh_for_event(&manager, &event);

        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(!watcher.recursive_roots[&root].contains(&ignored_dir));
            assert!(!watcher.path_watch_counts.contains_key(&ignored_dir));
        }
        manager.unwatch(&root).unwrap();
    }

    #[test]
    fn test_refresh_handles_gitignore_change() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let ignored_dir = root.join("ignored");
        let gitignore = root.join(".gitignore");
        fs::create_dir(root.join(".git")).unwrap();
        fs::create_dir_all(&ignored_dir).unwrap();
        fs::write(&gitignore, "ignored/\n").unwrap();

        let manager = test_manager();
        manager.watch(&root, true).unwrap();
        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(!watcher.recursive_roots[&root].contains(&ignored_dir));
        }

        fs::write(&gitignore, "").unwrap();
        let unignore_event =
            Event::new(EventKind::Modify(ModifyKind::Any)).add_path(gitignore.clone());
        refresh_for_event(&manager, &unignore_event);
        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots[&root].contains(&ignored_dir));
            assert_eq!(watcher.path_watch_counts.get(&ignored_dir), Some(&1));
        }

        fs::write(&gitignore, "ignored/\n").unwrap();
        let ignore_event =
            Event::new(EventKind::Modify(ModifyKind::Any)).add_path(gitignore.clone());
        refresh_for_event(&manager, &ignore_event);
        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(!watcher.recursive_roots[&root].contains(&ignored_dir));
            assert!(!watcher.path_watch_counts.contains_key(&ignored_dir));
        }
        manager.unwatch(&root).unwrap();
    }

    #[test]
    fn test_refresh_removes_deleted_directory_and_rewatches_recreated_path() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let removed_dir = root.join("removed");
        fs::create_dir_all(&removed_dir).unwrap();

        let manager = test_manager();
        manager.watch(&root, true).unwrap();
        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots[&root].contains(&removed_dir));
            assert_eq!(watcher.path_watch_counts.get(&removed_dir), Some(&1));
        }

        fs::remove_dir_all(&removed_dir).unwrap();
        let remove_event =
            Event::new(EventKind::Remove(RemoveKind::Folder)).add_path(removed_dir.clone());
        refresh_for_event(&manager, &remove_event);
        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(!watcher.recursive_roots[&root].contains(&removed_dir));
            assert!(!watcher.path_watch_counts.contains_key(&removed_dir));
        }

        fs::create_dir_all(&removed_dir).unwrap();
        let create_event =
            Event::new(EventKind::Create(CreateKind::Folder)).add_path(removed_dir.clone());
        refresh_for_event(&manager, &create_event);
        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots[&root].contains(&removed_dir));
            assert_eq!(watcher.path_watch_counts.get(&removed_dir), Some(&1));
        }

        manager.unwatch(&root).unwrap();
    }

    #[test]
    fn test_refresh_handles_directory_rename() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let old_dir = root.join("old");
        let new_dir = root.join("new");
        fs::create_dir_all(&old_dir).unwrap();

        let manager = test_manager();
        manager.watch(&root, true).unwrap();
        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots[&root].contains(&old_dir));
            assert_eq!(watcher.path_watch_counts.get(&old_dir), Some(&1));
        }

        fs::rename(&old_dir, &new_dir).unwrap();
        let event = Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::Both)))
            .add_path(old_dir.clone())
            .add_path(new_dir.clone());
        refresh_for_event(&manager, &event);

        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(!watcher.recursive_roots[&root].contains(&old_dir));
            assert!(watcher.recursive_roots[&root].contains(&new_dir));
            assert!(!watcher.path_watch_counts.contains_key(&old_dir));
            assert_eq!(watcher.path_watch_counts.get(&new_dir), Some(&1));
        }
        manager.unwatch(&root).unwrap();
    }

    #[cfg(all(target_os = "linux", unix))]
    #[test]
    fn test_recursive_watch_follows_symlinked_directories_for_real_events() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let real = root.join("real");
        let link = root.join("link");
        let linked_nested = link.join("nested");
        let linked_file = linked_nested.join("file");
        fs::create_dir_all(real.join("nested")).unwrap();
        std::os::unix::fs::symlink(&real, &link).unwrap();

        let (tx, rx) = std_mpsc::channel();
        let mut watcher = FilteredWatcher::new(move |event: notify::Result<Event>| {
            if let Ok(event) = event {
                let _ = tx.send(event);
            }
        })
        .unwrap();

        watcher.watch(&root, RecursiveMode::Recursive).unwrap();
        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::write(&linked_file, "changed").unwrap();
        recv_event_matching(&rx, Duration::from_secs(2), |event| {
            event
                .paths
                .iter()
                .any(|path| path.starts_with(&linked_nested))
        });

        watcher.unwatch(&root).unwrap();
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_refresh_rewatches_renamed_directory_for_real_events() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let old_dir = root.join("old");
        let new_dir = root.join("new");
        let new_file = new_dir.join("file");
        fs::create_dir_all(&old_dir).unwrap();

        let (tx, rx) = std_mpsc::channel();
        let mut watcher = FilteredWatcher::new(move |event: notify::Result<Event>| {
            if let Ok(event) = event {
                let _ = tx.send(event);
            }
        })
        .unwrap();

        watcher.watch(&root, RecursiveMode::Recursive).unwrap();
        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::rename(&old_dir, &new_dir).unwrap();
        let dirs = FilteredWatcher::collect_recursive_dirs(&root);
        watcher.apply_recursive_dirs(&root, dirs).unwrap();
        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::write(&new_file, "changed").unwrap();
        recv_event_matching(&rx, Duration::from_secs(2), |event| {
            event.paths.iter().any(|path| path.starts_with(&new_dir))
        });

        watcher.unwatch(&root).unwrap();
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_overlapping_roots_rewatch_renamed_dir_for_real_events() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let sub = root.join("sub");
        let old_dir = sub.join("old");
        let new_dir = sub.join("new");
        let new_file = new_dir.join("file");
        fs::create_dir_all(&old_dir).unwrap();

        let (tx, rx) = std_mpsc::channel();
        let mut watcher = FilteredWatcher::new(move |event: notify::Result<Event>| {
            if let Ok(event) = event {
                let _ = tx.send(event);
            }
        })
        .unwrap();

        watcher.watch(&root, RecursiveMode::Recursive).unwrap();
        watcher.watch(&sub, RecursiveMode::Recursive).unwrap();
        assert_eq!(watcher.path_watch_counts.get(&old_dir), Some(&2));

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::rename(&old_dir, &new_dir).unwrap();
        for r in [&root, &sub] {
            let dirs = FilteredWatcher::collect_recursive_dirs(r);
            watcher.apply_recursive_dirs(r, dirs).unwrap();
        }
        assert_eq!(watcher.path_watch_counts.get(&new_dir), Some(&2));
        assert!(!watcher.path_watch_counts.contains_key(&old_dir));

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::write(&new_file, "changed").unwrap();
        recv_event_matching(&rx, Duration::from_secs(2), |event| {
            event.paths.iter().any(|path| path.starts_with(&new_dir))
        });

        watcher.unwatch(&root).unwrap();
        watcher.unwatch(&sub).unwrap();
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_overlap_delete_recreate_rewatches_for_real_events() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let sub = root.join("sub");
        let sub_file = sub.join("file");
        fs::create_dir_all(&sub).unwrap();

        let (tx, rx) = std_mpsc::channel();
        let mut watcher = FilteredWatcher::new(move |event: notify::Result<Event>| {
            if let Ok(event) = event {
                let _ = tx.send(event);
            }
        })
        .unwrap();

        watcher.watch(&sub, RecursiveMode::NonRecursive).unwrap();
        watcher.watch(&root, RecursiveMode::Recursive).unwrap();
        assert_eq!(watcher.path_watch_counts.get(&sub), Some(&2));

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::remove_dir_all(&sub).unwrap();
        let dirs = FilteredWatcher::collect_recursive_dirs(&root);
        watcher.apply_recursive_dirs(&root, dirs).unwrap();

        fs::create_dir(&sub).unwrap();
        let dirs = FilteredWatcher::collect_recursive_dirs(&root);
        watcher.apply_recursive_dirs(&root, dirs).unwrap();

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::write(&sub_file, "changed").unwrap();
        recv_event_matching(&rx, Duration::from_secs(2), |event| {
            event.paths.iter().any(|path| path.starts_with(&sub))
        });

        let _ = watcher.unwatch(&root);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_coalesced_delete_recreate_within_window_rewatches_for_real_events() {
        use notify::event::CreateKind;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let sub = root.join("sub");
        let nested = sub.join("nested");
        let nested_file = nested.join("file");
        fs::create_dir_all(&nested).unwrap();

        let (tx, rx) = std_mpsc::channel();
        let manager = WatchManager {
            watcher: Mutex::new(
                FilteredWatcher::new(move |event: notify::Result<Event>| {
                    if let Ok(event) = event {
                        let _ = tx.send(event);
                    }
                })
                .unwrap(),
            ),
            watched_paths: Mutex::new(HashMap::new()),
        };
        manager.watch(&root, true).unwrap();

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::remove_dir_all(&sub).unwrap();
        fs::create_dir_all(&nested).unwrap();

        // Drive debounce_loop's post-window refresh for coalesced Remove+Create.
        let remove_event = Event::new(EventKind::Remove(RemoveKind::Folder)).add_path(sub.clone());
        let create_event = Event::new(EventKind::Create(CreateKind::Folder)).add_path(sub.clone());
        let mut roots: HashSet<PathBuf> = HashSet::new();
        let mut suspects: HashSet<PathBuf> = HashSet::new();
        for event in [&remove_event, &create_event] {
            roots.extend(manager.recursive_roots_for_event(event));
            suspects.extend(inode_replacing_paths(event));
        }
        manager.refresh_recursive_roots(roots);
        manager.rearm_suspect_paths(suspects);

        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots[&root].contains(&sub));
            assert!(watcher.recursive_roots[&root].contains(&nested));
            assert_eq!(watcher.path_watch_counts.get(&sub), Some(&1));
            assert_eq!(watcher.path_watch_counts.get(&nested), Some(&1));
        }

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::write(&nested_file, "changed").unwrap();
        recv_event_matching(&rx, Duration::from_secs(2), |event| {
            event.paths.iter().any(|path| path.starts_with(&nested))
        });

        manager.unwatch(&root).unwrap();
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_coalesced_rename_into_place_within_window_rewatches_for_real_events() {
        use notify::event::RenameMode;

        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let sub = root.join("sub");
        let sub_file = sub.join("file");
        fs::create_dir(&sub).unwrap();

        let staging_temp = tempfile::tempdir().unwrap();
        let staging = staging_temp.path().join("staging");
        fs::create_dir(&staging).unwrap();

        let (tx, rx) = std_mpsc::channel();
        let manager = WatchManager {
            watcher: Mutex::new(
                FilteredWatcher::new(move |event: notify::Result<Event>| {
                    if let Ok(event) = event {
                        let _ = tx.send(event);
                    }
                })
                .unwrap(),
            ),
            watched_paths: Mutex::new(HashMap::new()),
        };
        manager.watch(&root, true).unwrap();

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::rename(&staging, &sub).unwrap();

        // Drive debounce_loop's post-window refresh for rename-into-place.
        let rename_event =
            Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::To))).add_path(sub.clone());
        let mut roots: HashSet<PathBuf> = HashSet::new();
        let mut suspects: HashSet<PathBuf> = HashSet::new();
        roots.extend(manager.recursive_roots_for_event(&rename_event));
        suspects.extend(inode_replacing_paths(&rename_event));
        manager.refresh_recursive_roots(roots);
        manager.rearm_suspect_paths(suspects);

        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots[&root].contains(&sub));
            assert_eq!(watcher.path_watch_counts.get(&sub), Some(&1));
        }

        std::thread::sleep(Duration::from_millis(100));
        drain_events(&rx);

        fs::write(&sub_file, "changed").unwrap();
        recv_event_matching(&rx, Duration::from_secs(2), |event| {
            event.paths.iter().any(|path| path.starts_with(&sub))
        });

        manager.unwatch(&root).unwrap();
    }

    #[test]
    fn test_watch_is_idempotent_for_same_path_and_upgrades_to_recursive() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();

        let manager = test_manager();
        manager.watch(&root, false).unwrap();
        manager.watch(&root, true).unwrap();
        manager.watch(&root, false).unwrap();

        {
            let watcher = lock_or_recover(&manager.watcher);
            assert!(watcher.recursive_roots.contains_key(&root));
            assert!(!watcher.direct_watches.contains(&root));
            assert_eq!(watcher.path_watch_counts.get(&root), Some(&1));
        }

        manager.unwatch(&root).unwrap();
        let watcher = lock_or_recover(&manager.watcher);
        assert!(!watcher.path_watch_counts.contains_key(&root));
    }

    #[test]
    fn test_recursive_unwatch_preserves_overlapping_direct_watch() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let sub = root.join("sub");
        fs::create_dir_all(&sub).unwrap();

        let mut watcher = FilteredWatcher::new(|_: notify::Result<Event>| {}).unwrap();
        watcher.watch(&sub, RecursiveMode::NonRecursive).unwrap();
        watcher.watch(&root, RecursiveMode::Recursive).unwrap();

        assert_eq!(watcher.path_watch_counts.get(&sub), Some(&2));
        watcher.unwatch(&root).unwrap();
        assert!(watcher.direct_watches.contains(&sub));
        assert_eq!(watcher.path_watch_counts.get(&sub), Some(&1));

        watcher.unwatch(&sub).unwrap();
        assert!(!watcher.path_watch_counts.contains_key(&sub));
    }

    #[test]
    fn test_recursive_unwatch_preserves_overlapping_recursive_watch() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let sub = root.join("sub");
        fs::create_dir_all(&sub).unwrap();

        let mut watcher = FilteredWatcher::new(|_: notify::Result<Event>| {}).unwrap();
        watcher.watch(&root, RecursiveMode::Recursive).unwrap();
        watcher.watch(&sub, RecursiveMode::Recursive).unwrap();

        assert_eq!(watcher.path_watch_counts.get(&sub), Some(&2));
        watcher.unwatch(&root).unwrap();
        assert!(watcher.recursive_roots.contains_key(&sub));
        assert_eq!(watcher.path_watch_counts.get(&sub), Some(&1));

        watcher.unwatch(&sub).unwrap();
        assert!(!watcher.path_watch_counts.contains_key(&sub));
    }
}
