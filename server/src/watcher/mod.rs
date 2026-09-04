// SPDX-License-Identifier: GPL-3.0-or-later

//! Filesystem watcher for cache invalidation notifications.
//!
//! Uses inotify (Linux) / kqueue (macOS) via the `notify` crate to watch
//! directories for changes. When changes are detected, a debounced
//! notification is sent to the Emacs client so it can invalidate its caches.

use crate::protocol::{Notification, RpcError};
use crate::{WriterHandle, msgpack_map};
use notify::event::{DataChange, MetadataKind, ModifyKind, RemoveKind, RenameMode};
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use rmpv::Value;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, OnceLock, Weak};
use tokio::io::AsyncWriteExt;
use tokio::sync::{Notify, mpsc};
use tokio::time::{self, Duration};

use crate::handlers::file::bytes_to_path;
use crate::protocol::{from_value, path_or_bytes};

/// Duration to debounce filesystem events before sending a notification.
/// During bulk operations (e.g. git checkout), many events fire in rapid
/// succession. We collect them all and send a single notification.
const DEBOUNCE_DURATION: Duration = Duration::from_millis(200);

/// Bound callback-to-debounce buffering during filesystem event storms.
const WATCH_INPUT_CAPACITY: usize = 4096;

/// Bound one public notification batch.  Overflow is represented by a single
/// rescan event, which tells the client to discard connection-local caches.
const MAX_DEBOUNCE_EVENTS: usize = 8192;

/// Global WatchManager instance, initialized in main().
static WATCH_MANAGER: OnceLock<Arc<WatchManager>> = OnceLock::new();

/// Whether the global watcher actually initialized.  `system.info.watcher`
/// previously reported `RecommendedWatcher::kind()` even when `init` failed,
/// so the client could not reliably shorten caches.  This tracks reality.
static WATCHER_ACTIVE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Get the global WatchManager, if initialized.
pub fn get() -> Option<&'static Arc<WatchManager>> {
    WATCH_MANAGER.get()
}

/// Whether filesystem push notifications are actually running.
pub fn is_active() -> bool {
    WATCHER_ACTIVE.load(std::sync::atomic::Ordering::Relaxed)
}

/// Initialize the global WatchManager. Called once from main().
pub fn init(manager: Arc<WatchManager>) {
    let _ = WATCH_MANAGER.set(manager);
    WATCHER_ACTIVE.store(true, std::sync::atomic::Ordering::Relaxed);
}

/// Record that watcher initialization failed; client should treat caches as
/// TTL-only without push invalidation.
pub fn set_unavailable() {
    WATCHER_ACTIVE.store(false, std::sync::atomic::Ordering::Relaxed);
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

    fn recursive_roots(&self) -> Vec<PathBuf> {
        self.recursive_roots.keys().cloned().collect()
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct WatchEvent {
    action: &'static str,
    path: Option<PathBuf>,
    path1: Option<PathBuf>,
    cookie: Option<usize>,
}

impl WatchEvent {
    fn path(action: &'static str, path: PathBuf) -> Self {
        Self {
            action,
            path: Some(path),
            path1: None,
            cookie: None,
        }
    }

    fn rename(path: PathBuf, path1: PathBuf) -> Self {
        Self {
            action: "renamed",
            path: Some(path),
            path1: Some(path1),
            cookie: None,
        }
    }

    fn tracked(action: &'static str, path: PathBuf, cookie: Option<usize>) -> Self {
        Self {
            action,
            path: Some(path),
            path1: None,
            cookie,
        }
    }

    fn rescan() -> Self {
        Self {
            action: "rescan",
            path: None,
            path1: None,
            cookie: None,
        }
    }

    fn to_value(&self) -> Value {
        let mut fields = vec![(
            Value::String("action".into()),
            Value::String(self.action.into()),
        )];

        if let Some(path) = &self.path {
            fields.push((Value::String("path".into()), path_to_value(path)));
        }
        if let Some(path1) = &self.path1 {
            fields.push((Value::String("path1".into()), path_to_value(path1)));
        }
        if let Some(cookie) = self.cookie {
            fields.push((Value::String("cookie".into()), Value::from(cookie as u64)));
        }

        Value::Map(fields)
    }
}

enum WatchInput {
    Notify(Event),
    #[cfg(any(target_os = "linux", target_os = "android"))]
    Direct(Vec<WatchEvent>),
}

#[derive(Clone)]
struct WatchInputSender {
    tx: mpsc::Sender<WatchInput>,
    overflowed: Arc<AtomicBool>,
    overflow_notify: Arc<Notify>,
}

impl WatchInputSender {
    fn send(&self, input: WatchInput) {
        match self.tx.try_send(input) {
            Ok(()) | Err(mpsc::error::TrySendError::Closed(_)) => {}
            Err(mpsc::error::TrySendError::Full(_)) => {
                self.overflowed.store(true, Ordering::Release);
                // A dropped input cannot wake the receiver through the full
                // channel.  Notify independently so a storm that ends at this
                // exact point still produces a rescan.
                self.overflow_notify.notify_one();
            }
        }
    }
}

fn path_to_value(path: &Path) -> Value {
    use std::os::unix::ffi::OsStrExt;

    Value::Binary(path.as_os_str().as_bytes().to_vec())
}

#[cfg(any(target_os = "linux", target_os = "android"))]
use rustix::fs::inotify;
#[cfg(any(target_os = "linux", target_os = "android"))]
use std::os::fd::OwnedFd;

#[cfg(any(target_os = "linux", target_os = "android"))]
struct NofollowSymlinkWatcher {
    fd: Arc<OwnedFd>,
    watches: HashMap<PathBuf, SymlinkWatchEntry>,
    wd_to_path: Arc<Mutex<HashMap<i32, PathBuf>>>,
    ignored_wds: Arc<Mutex<HashSet<i32>>>,
    running: Arc<std::sync::atomic::AtomicBool>,
    reader: Option<std::thread::JoinHandle<()>>,
}

#[cfg(any(target_os = "linux", target_os = "android"))]
#[derive(Debug, Clone, Copy)]
struct SymlinkWatchEntry {
    wd: i32,
    count: usize,
}

#[cfg(any(target_os = "linux", target_os = "android"))]
impl NofollowSymlinkWatcher {
    fn new(tx: WatchInputSender) -> Result<Self, notify::Error> {
        let fd = inotify::init(inotify::CreateFlags::NONBLOCK | inotify::CreateFlags::CLOEXEC)
            .map(Arc::new)
            .map_err(|err| notify::Error::io(err.into()))?;

        let wd_to_path = Arc::new(Mutex::new(HashMap::new()));
        let ignored_wds = Arc::new(Mutex::new(HashSet::new()));
        let running = Arc::new(std::sync::atomic::AtomicBool::new(true));
        let reader = Some(spawn_nofollow_reader(
            Arc::clone(&fd),
            Arc::clone(&wd_to_path),
            Arc::clone(&ignored_wds),
            Arc::clone(&running),
            tx,
        ));

        Ok(Self {
            fd,
            watches: HashMap::new(),
            wd_to_path,
            ignored_wds,
            running,
            reader,
        })
    }

    fn contains(&mut self, path: &Path) -> bool {
        self.purge_ignored();
        self.watches.contains_key(path)
    }

    fn watch(&mut self, path: &Path) -> Result<PathBuf, notify::Error> {
        self.purge_ignored();
        let path = path.to_path_buf();
        if let Some(entry) = self.watches.get_mut(&path) {
            entry.count += 1;
            return Ok(path);
        }

        let metadata = std::fs::symlink_metadata(&path)
            .map_err(|err| notify::Error::io(err).add_path(path.clone()))?;
        if !metadata.file_type().is_symlink() {
            return Err(notify::Error::generic(&format!(
                "nofollow watch path is not a symlink: {}",
                path.display()
            )));
        }

        // Only request symlink metadata changes.  Emacs' symlink tests expect
        // no target events and no event when the symlink is deleted as part of
        // cleanup, but do expect `set-file-times' with nofollow to report an
        // attribute change.
        let mask = inotify::WatchFlags::ATTRIB | inotify::WatchFlags::DONT_FOLLOW;
        let wd = inotify::add_watch(&*self.fd, &path, mask)
            .map_err(|err| notify::Error::io(err.into()).add_path(path.clone()))?;

        self.watches
            .insert(path.clone(), SymlinkWatchEntry { wd, count: 1 });
        lock_or_recover(&self.wd_to_path).insert(wd, path.clone());
        Ok(path)
    }

    fn unwatch(&mut self, path: &Path) -> Result<(), notify::Error> {
        self.purge_ignored();
        let Some(entry) = self.watches.get_mut(path) else {
            return Err(notify::Error::watch_not_found().add_path(path.to_path_buf()));
        };

        if entry.count > 1 {
            entry.count -= 1;
            return Ok(());
        }

        let wd = entry.wd;
        inotify::remove_watch(&*self.fd, wd)
            .map_err(|err| notify::Error::io(err.into()).add_path(path.to_path_buf()))?;

        self.watches.remove(path);
        lock_or_recover(&self.wd_to_path).remove(&wd);
        Ok(())
    }

    fn purge_ignored(&mut self) {
        let ignored: Vec<_> = lock_or_recover(&self.ignored_wds).drain().collect();
        if ignored.is_empty() {
            return;
        }

        let mut wd_to_path = lock_or_recover(&self.wd_to_path);
        for wd in ignored {
            if let Some(path) = self
                .watches
                .iter()
                .find_map(|(path, entry)| (entry.wd == wd).then(|| path.clone()))
            {
                self.watches.remove(&path);
            }
            wd_to_path.remove(&wd);
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
impl Drop for NofollowSymlinkWatcher {
    fn drop(&mut self) {
        self.running
            .store(false, std::sync::atomic::Ordering::Relaxed);
        if let Some(reader) = self.reader.take() {
            let _ = reader.join();
        }
        // The last `Arc<OwnedFd>` clone (ours, once the reader has joined)
        // closes the inotify descriptor.
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn spawn_nofollow_reader(
    fd: Arc<OwnedFd>,
    wd_to_path: Arc<Mutex<HashMap<i32, PathBuf>>>,
    ignored_wds: Arc<Mutex<HashSet<i32>>>,
    running: Arc<std::sync::atomic::AtomicBool>,
    tx: WatchInputSender,
) -> std::thread::JoinHandle<()> {
    std::thread::spawn(move || {
        let mut buf = [std::mem::MaybeUninit::<u8>::uninit(); 4096];
        let mut events = inotify::Reader::new(&*fd, &mut buf);
        while running.load(std::sync::atomic::Ordering::Relaxed) {
            // Drain every buffered event before sleeping: the nonblocking
            // descriptor reports AGAIN once no more events are pending.
            let drained = loop {
                match events.next() {
                    Ok(event) => emit_nofollow_event(&event, &wd_to_path, &ignored_wds, &tx),
                    Err(rustix::io::Errno::AGAIN) => break true,
                    Err(rustix::io::Errno::INTR) => break true,
                    Err(_) => break false,
                }
            };
            if !drained {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(25));
        }
    })
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn emit_nofollow_event(
    event: &inotify::Event<'_>,
    wd_to_path: &Mutex<HashMap<i32, PathBuf>>,
    ignored_wds: &Mutex<HashSet<i32>>,
    tx: &WatchInputSender,
) {
    let wd = event.wd();
    let mut paths = lock_or_recover(wd_to_path);
    let path = paths.get(&wd).cloned();
    if event.events().contains(inotify::ReadFlags::IGNORED) {
        paths.remove(&wd);
        lock_or_recover(ignored_wds).insert(wd);
    }
    drop(paths);

    if event.events().contains(inotify::ReadFlags::ATTRIB)
        && let Some(path) = path
    {
        tx.send(WatchInput::Direct(vec![WatchEvent::path(
            "attribute-changed",
            path,
        )]));
    }
}

#[cfg(not(any(target_os = "linux", target_os = "android")))]
struct NofollowSymlinkWatcher;

#[cfg(not(any(target_os = "linux", target_os = "android")))]
impl NofollowSymlinkWatcher {
    fn new(_tx: WatchInputSender) -> Result<Self, notify::Error> {
        Ok(Self)
    }

    fn contains(&mut self, _path: &Path) -> bool {
        false
    }

    fn watch(&mut self, path: &Path) -> Result<PathBuf, notify::Error> {
        Err(notify::Error::generic(&format!(
            "nofollow symlink watches are not supported on this platform: {}",
            path.display()
        )))
    }

    fn unwatch(&mut self, path: &Path) -> Result<(), notify::Error> {
        Err(notify::Error::watch_not_found().add_path(path.to_path_buf()))
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

    /// Nofollow symlink watches for file-notify descriptors.
    symlink_watcher: Mutex<Option<NofollowSymlinkWatcher>>,
}

impl WatchManager {
    /// Create a new WatchManager and spawn the debounce background task.
    ///
    /// The debounce task receives raw inotify events, batches them over a
    /// short window, and writes `fs.events` notifications to the client
    /// via the shared stdout writer.
    pub fn new(writer: WriterHandle) -> Result<Arc<Self>, notify::Error> {
        let (tx, rx) = mpsc::channel(WATCH_INPUT_CAPACITY);
        let overflowed = Arc::new(AtomicBool::new(false));
        let overflow_notify = Arc::new(Notify::new());
        let input = WatchInputSender {
            tx,
            overflowed: Arc::clone(&overflowed),
            overflow_notify: Arc::clone(&overflow_notify),
        };
        let notify_input = input.clone();

        let watcher = FilteredWatcher::new(move |event: notify::Result<Event>| {
            if let Ok(event) = event
                && (matches!(
                    event.kind,
                    EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_)
                ) || event.need_rescan())
            {
                // Never block notify's callback thread.  Queue overflow is
                // coalesced into a rescan notification by the debounce task.
                notify_input.send(WatchInput::Notify(event));
            }
        })?;

        let manager = Arc::new(Self {
            watcher: Mutex::new(watcher),
            watched_paths: Mutex::new(HashMap::new()),
            symlink_watcher: Mutex::new(NofollowSymlinkWatcher::new(input).ok()),
        });

        // Spawn the debounce background task.
        tokio::spawn(debounce_loop(
            rx,
            overflowed,
            overflow_notify,
            writer,
            Arc::downgrade(&manager),
        ));

        Ok(manager)
    }

    /// Start watching a path for filesystem changes.
    ///
    /// If `recursive` is true, all subdirectories are also watched.
    /// Returns the canonical watched path, or an error if the path doesn't
    /// exist or watch limits are exceeded.
    ///
    /// Repeated watches are idempotent; non-recursive watches can be upgraded.
    pub fn watch(&self, path: &Path, recursive: bool) -> Result<PathBuf, notify::Error> {
        self.watch_with_options(path, recursive, false)
    }

    /// Start watching a path, optionally without following a symlink path.
    pub fn watch_with_options(
        &self,
        path: &Path,
        recursive: bool,
        nofollow: bool,
    ) -> Result<PathBuf, notify::Error> {
        if nofollow {
            let mut watcher = lock_or_recover(&self.symlink_watcher);
            let Some(watcher) = watcher.as_mut() else {
                return Err(notify::Error::generic(
                    "nofollow symlink watches are not available",
                ));
            };
            return watcher.watch(path);
        }

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
            Some(existing) if existing == mode => return Ok(canonical),
            Some(RecursiveMode::Recursive) => return Ok(canonical),
            Some(RecursiveMode::NonRecursive) => {
                if let Err(err) = watcher.unwatch(&canonical) {
                    // notify drops its own backend registration before the
                    // unwatch syscall reports failure, so an error here can
                    // still leave the path unwatched.  Restore a consistent
                    // non-recursive registration before surfacing the error,
                    // otherwise this map claims a live watch that is gone.
                    // rearm_existing_watch forces a backend re-registration;
                    // watch_nonrecursive would return early because the
                    // logical direct_watches entry still exists.
                    if watcher.rearm_existing_watch(&canonical).is_ok() {
                        paths.insert(canonical.clone(), RecursiveMode::NonRecursive);
                    } else {
                        paths.remove(&canonical);
                    }
                    return Err(err);
                }
                if let Err(err) = watcher.watch(&canonical, RecursiveMode::Recursive) {
                    if watcher
                        .watch(&canonical, RecursiveMode::NonRecursive)
                        .is_err()
                    {
                        paths.remove(&canonical);
                    }
                    return Err(err);
                }
                paths.insert(canonical.clone(), RecursiveMode::Recursive);
            }
            None => {
                watcher.watch(&canonical, mode)?;
                paths.insert(canonical.clone(), mode);
            }
        }

        Ok(canonical)
    }

    /// Stop watching a path.
    ///
    /// Looks up the stored canonical path from when watch() was called,
    /// so this works even if the directory has been deleted since then.
    ///
    /// Lock ordering: watcher -> watched_paths (same as watch()).
    pub fn unwatch(&self, path: &Path) -> Result<(), notify::Error> {
        {
            let mut symlink_watcher = lock_or_recover(&self.symlink_watcher);
            if let Some(watcher) = symlink_watcher.as_mut()
                && watcher.contains(path)
            {
                return watcher.unwatch(path);
            }
        }

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

    fn overflow_recovery_paths(&self) -> (HashSet<PathBuf>, HashSet<PathBuf>) {
        let recursive_roots = lock_or_recover(&self.watcher)
            .recursive_roots()
            .into_iter()
            .collect();
        let watched_paths = lock_or_recover(&self.watched_paths)
            .keys()
            .cloned()
            .collect();
        (recursive_roots, watched_paths)
    }

    fn recursive_roots_for_event(&self, event: &Event) -> HashSet<PathBuf> {
        let refresh_paths = directory_tree_refresh_paths(event);
        let ignore_paths = ignore_rule_paths(event);
        let need_rescan = event.need_rescan();
        if !need_rescan && refresh_paths.is_empty() && ignore_paths.is_empty() {
            return HashSet::new();
        }

        let watcher = lock_or_recover(&self.watcher);
        let mut roots_to_refresh: HashSet<_> = if need_rescan {
            watcher.recursive_roots().into_iter().collect()
        } else {
            watcher
                .recursive_roots_for_paths(&refresh_paths)
                .into_iter()
                .collect()
        };

        for path in ignore_paths {
            roots_to_refresh.extend(watcher.recursive_roots_for_ignore_rule(&path));
        }

        roots_to_refresh
    }

    async fn refresh_recursive_roots(&self, roots_to_refresh: HashSet<PathBuf>) {
        if roots_to_refresh.is_empty() {
            return;
        }

        // Tree scans can take a long time on large or slow filesystems; run
        // them off the Tokio workers that drive request dispatch and response
        // writes.
        let scanned = tokio::task::spawn_blocking(move || {
            roots_to_refresh
                .into_iter()
                .map(|root| {
                    let dirs = FilteredWatcher::collect_recursive_dirs(&root);
                    (root, dirs)
                })
                .collect::<Vec<_>>()
        })
        .await
        .unwrap_or_default();

        let mut watcher = lock_or_recover(&self.watcher);
        for (root, dirs) in scanned {
            let _ = watcher.apply_recursive_dirs(&root, dirs);
        }
    }

    /// Rebind still-existing watches after Linux/inotify inode replacement.
    ///
    /// Must run after `refresh_recursive_roots`, so genuine deletes are already
    /// pruned and only path-identical replacements remain.
    async fn rearm_suspect_paths(&self, suspect_paths: HashSet<PathBuf>) {
        let suspects: Vec<PathBuf> = suspect_paths.into_iter().collect();
        let candidates = {
            let watcher = lock_or_recover(&self.watcher);
            watcher.watched_paths_under(&suspects)
        };
        // The liveness probes below stat every candidate path, which can be a
        // storm on large watches; run them off the Tokio workers.
        let existing = tokio::task::spawn_blocking(move || {
            candidates
                .into_iter()
                .filter(|path| path.is_dir())
                .collect::<Vec<_>>()
        })
        .await
        .unwrap_or_default();
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

fn event_to_watch_events(event: &Event) -> Vec<WatchEvent> {
    let mut events = Vec::new();

    if event.need_rescan() {
        events.push(WatchEvent::rescan());
    }

    match event.kind {
        EventKind::Create(_) => {
            events.extend(paths_as_events("created", &event.paths));
        }
        EventKind::Modify(ModifyKind::Data(
            DataChange::Any | DataChange::Size | DataChange::Content | DataChange::Other,
        ))
        | EventKind::Modify(ModifyKind::Any | ModifyKind::Other) => {
            events.extend(paths_as_events("changed", &event.paths));
        }
        EventKind::Modify(ModifyKind::Metadata(
            MetadataKind::Any
            | MetadataKind::AccessTime
            | MetadataKind::WriteTime
            | MetadataKind::Permissions
            | MetadataKind::Ownership
            | MetadataKind::Extended
            | MetadataKind::Other,
        )) => {
            events.extend(paths_as_events("attribute-changed", &event.paths));
        }
        EventKind::Modify(ModifyKind::Name(RenameMode::Both)) => {
            if event.paths.len() >= 2 {
                if let (Some(from), Some(to)) = (event.paths.first(), event.paths.last()) {
                    events.push(WatchEvent::rename(from.clone(), to.clone()));
                }
            } else {
                events.extend(paths_as_events("renamed", &event.paths));
            }
        }
        EventKind::Modify(ModifyKind::Name(RenameMode::From)) => {
            let cookie = event.tracker();
            events.extend(
                event
                    .paths
                    .iter()
                    .cloned()
                    .map(|path| WatchEvent::tracked("renamed-from", path, cookie)),
            );
        }
        EventKind::Modify(ModifyKind::Name(RenameMode::To)) => {
            let cookie = event.tracker();
            events.extend(
                event
                    .paths
                    .iter()
                    .cloned()
                    .map(|path| WatchEvent::tracked("renamed-to", path, cookie)),
            );
        }
        EventKind::Modify(ModifyKind::Name(RenameMode::Any | RenameMode::Other)) => {
            events.extend(paths_as_events("renamed", &event.paths));
        }
        EventKind::Remove(_) => {
            events.extend(paths_as_events("deleted", &event.paths));
        }
        EventKind::Any | EventKind::Access(_) | EventKind::Other => {}
    }

    events
}

fn paths_as_events<'paths>(
    action: &'static str,
    paths: &'paths [PathBuf],
) -> impl Iterator<Item = WatchEvent> + 'paths {
    paths
        .iter()
        .cloned()
        .map(move |path| WatchEvent::path(action, path))
}

/// Background task: receives raw inotify events, debounces them, and sends
/// batched `fs.events` notifications to the Emacs client.
///
/// Algorithm (fixed-window debounce):
/// 1. Wait for the first event (blocks until something happens)
/// 2. Start a 200ms timer
/// 3. Collect all events that arrive during the timer window
/// 4. When the timer fires, send one notification with all collected events
/// 5. Go back to step 1
async fn debounce_loop(
    mut rx: mpsc::Receiver<WatchInput>,
    overflowed: Arc<AtomicBool>,
    overflow_notify: Arc<Notify>,
    writer: WriterHandle,
    manager: Weak<WatchManager>,
) {
    loop {
        // Phase 1: Wait for the first event or an overflow wakeup.  The latter
        // is independent of the bounded channel because the dropped input that
        // set the overflow flag could not be queued.
        let event = tokio::select! {
            event = rx.recv() => match event {
                Some(event) => Some(event),
                None => break,
            },
            () = overflow_notify.notified() => None,
        };

        let mut pending_events: Vec<WatchEvent> = Vec::new();
        let mut roots_to_refresh: HashSet<PathBuf> = HashSet::new();
        let mut suspect_paths: HashSet<PathBuf> = HashSet::new();
        let mut batch_overflowed = event.is_some_and(|event| {
            collect_input(
                event,
                &manager,
                &mut pending_events,
                &mut roots_to_refresh,
                &mut suspect_paths,
            )
        });

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
                            if !batch_overflowed {
                                batch_overflowed = collect_input(
                                    e,
                                    &manager,
                                    &mut pending_events,
                                    &mut roots_to_refresh,
                                    &mut suspect_paths,
                                );
                            }
                        }
                        None => return, // Channel closed
                    }
                }
                () = overflow_notify.notified() => {
                    // The atomic flag is consumed once after the debounce
                    // window together with any ordinary queued events.
                }
            }
        }

        // Always consume the sender flag.  Short-circuiting this swap when the
        // in-memory batch overflowed would cause a redundant rescan next time.
        let sender_overflowed = overflowed.swap(false, Ordering::AcqRel);
        let overflowed = batch_overflowed || sender_overflowed;
        if let Some(manager) = manager.upgrade() {
            if overflowed {
                let (all_recursive_roots, all_watched_paths) = manager.overflow_recovery_paths();
                roots_to_refresh.extend(all_recursive_roots);
                suspect_paths.extend(all_watched_paths);
            }
            manager.refresh_recursive_roots(roots_to_refresh).await;
            manager.rearm_suspect_paths(suspect_paths).await;
        }
        if overflowed {
            coalesce_to_rescan(&mut pending_events);
        }

        // Phase 3: Send notification with all collected events
        if !pending_events.is_empty() && send_notification(&writer, &pending_events).await.is_err()
        {
            // Stdout is broken (Emacs disconnected), stop the loop.
            // Cannot use eprintln! as SSH merges stderr with stdout.
            break;
        }
    }
}

fn append_watch_events(pending_events: &mut Vec<WatchEvent>, events: Vec<WatchEvent>) -> bool {
    let remaining = MAX_DEBOUNCE_EVENTS.saturating_sub(pending_events.len());
    let overflowed = events.len() > remaining;
    pending_events.extend(events.into_iter().take(remaining));
    overflowed
}

fn coalesce_to_rescan(pending_events: &mut Vec<WatchEvent>) {
    pending_events.clear();
    pending_events.push(WatchEvent::rescan());
}

fn collect_input(
    input: WatchInput,
    manager: &Weak<WatchManager>,
    pending_events: &mut Vec<WatchEvent>,
    roots_to_refresh: &mut HashSet<PathBuf>,
    suspect_paths: &mut HashSet<PathBuf>,
) -> bool {
    match input {
        WatchInput::Notify(event) => {
            let rescan_required = event.need_rescan();
            if let Some(manager) = manager.upgrade() {
                roots_to_refresh.extend(manager.recursive_roots_for_event(&event));
            }

            suspect_paths.extend(inode_replacing_paths(&event));
            append_watch_events(pending_events, event_to_watch_events(&event)) || rescan_required
        }
        #[cfg(any(target_os = "linux", target_os = "android"))]
        WatchInput::Direct(events) => append_watch_events(pending_events, events),
    }
}

fn fs_events_notification(events: &[WatchEvent]) -> Notification {
    let events_value: Vec<Value> = events.iter().map(WatchEvent::to_value).collect();

    Notification::new(
        "fs.events",
        Value::Map(vec![(
            Value::String("events".into()),
            Value::Array(events_value),
        )]),
    )
}

async fn send_notification_with_limit<W>(
    writer: &Arc<tokio::sync::Mutex<W>>,
    events: &[WatchEvent],
    max_frame_size: usize,
) -> Result<(), Box<dyn std::error::Error>>
where
    W: tokio::io::AsyncWrite + Unpin,
{
    let mut batches = vec![events];
    while let Some(batch) = batches.pop() {
        if batch.is_empty() {
            continue;
        }
        let bytes = rmp_serde::to_vec_named(&fs_events_notification(batch))?;
        if bytes.len() > max_frame_size {
            if batch.len() == 1 {
                return Err("single fs.events entry exceeds maximum frame size".into());
            }
            let middle = batch.len() / 2;
            // Stack order is reversed so the original event order is retained.
            batches.push(&batch[middle..]);
            batches.push(&batch[..middle]);
            continue;
        }

        // Each frame is independently valid.  Release the shared writer after
        // every frame so normal RPC responses can make progress between parts
        // of a large notification batch.
        {
            let mut writer = writer.lock().await;
            writer
                .write_all(&(bytes.len() as u32).to_be_bytes())
                .await?;
            writer.write_all(&bytes).await?;
            writer.flush().await?;
        }
        tokio::task::yield_now().await;
    }
    Ok(())
}

/// Serialize and send bounded `fs.events` notifications over the stdout writer.
/// Oversized debounce batches are split and streamed in event order.
async fn send_notification(
    writer: &WriterHandle,
    events: &[WatchEvent],
) -> Result<(), Box<dyn std::error::Error>> {
    send_notification_with_limit(writer, events, crate::MAX_FRAME_SIZE).await
}

// ============================================================================
// RPC handlers for watch.add, watch.remove, watch.list
// ============================================================================

use crate::handlers::HandlerResult;

/// Handle `watch.add` - start watching a directory for changes.
///
/// Params: { "path": "/path/to/dir", "recursive": true|false,
/// "nofollow": true|false }
pub async fn handle_add(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        #[serde(default = "default_recursive")]
        recursive: bool,
        #[serde(default)]
        nofollow: bool,
    }
    fn default_recursive() -> bool {
        true
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // `bytes_to_path` preserves the legacy ~ expansion used by watch paths.
    let path = bytes_to_path(&params.path).await?;

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    // A recursive watch walks the whole directory tree; keep that off the
    // Tokio workers that drive request dispatch and response writes.
    let manager = Arc::clone(manager);
    let watch_path = path.clone();
    let recursive = params.recursive;
    let nofollow = params.nofollow;
    let canonical = tokio::task::spawn_blocking(move || {
        if nofollow {
            manager.watch_with_options(&watch_path, recursive, true)
        } else {
            manager.watch(&watch_path, recursive)
        }
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Task join error: {e}")))?
    .map_err(|e| RpcError::internal_error(format!("Failed to watch: {e}")))?;

    Ok(msgpack_map! {
        "path" => path_to_value(&canonical),
        "recursive" => Value::Boolean(recursive),
        "nofollow" => Value::Boolean(nofollow)
    })
}

/// Handle `watch.remove` - stop watching a directory.
///
/// Params: { "path": "/path/to/dir" }
pub async fn handle_remove(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // `bytes_to_path` preserves the legacy ~ expansion used by watch paths.
    let path = bytes_to_path(&params.path).await?;

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    let manager = Arc::clone(manager);
    tokio::task::spawn_blocking(move || manager.unwatch(&path))
        .await
        .map_err(|e| RpcError::internal_error(format!("Task join error: {e}")))?
        .map_err(|e| RpcError::internal_error(format!("Failed to unwatch: {e}")))?;

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
                "path" => path_to_value(&path),
                "recursive" => Value::Boolean(recursive)
            }
        })
        .collect();

    Ok(Value::Array(watches))
}

#[cfg(test)]
mod tests;
