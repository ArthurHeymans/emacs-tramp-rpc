// SPDX-License-Identifier: GPL-3.0-or-later

//! Filesystem watcher for cache invalidation notifications.
//!
//! Uses inotify (Linux) / kqueue (macOS) via the `notify` crate to watch
//! directories for changes. When changes are detected, a debounced
//! notification is sent to the Emacs client so it can invalidate its caches.

use crate::output::encode_framed;
use crate::protocol::{Notification, RpcError, RpcPath, from_value};
use crate::{WriterHandle, msgpack_map};
use notify::event::{DataChange, MetadataKind, ModifyKind, RemoveKind, RenameMode};
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use rmpv::Value;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, OnceLock, Weak};
use tokio::sync::{Notify, mpsc};
use tokio::time::{self, Duration};

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
static WATCHER_ACTIVE: AtomicBool = AtomicBool::new(false);

/// Get the global WatchManager, if initialized.
pub fn get() -> Option<&'static Arc<WatchManager>> {
    WATCH_MANAGER.get()
}

/// Whether filesystem push notifications are actually running.
pub fn is_active() -> bool {
    WATCHER_ACTIVE.load(Ordering::Relaxed)
}

/// Initialize the global WatchManager. Called once from main().
pub fn init(manager: Arc<WatchManager>) {
    let _ = WATCH_MANAGER.set(manager);
    WATCHER_ACTIVE.store(true, Ordering::Relaxed);
}

/// Record that watcher initialization failed; client should treat caches as
/// TTL-only without push invalidation.
pub fn set_unavailable() {
    WATCHER_ACTIVE.store(false, Ordering::Relaxed);
}

/// Helper to lock a std::sync::Mutex, recovering from poisoning.
/// The data is still valid after a panic, so we just unwrap the poison error.
fn lock_or_recover<T>(mutex: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(|e| e.into_inner())
}

/// Recursive roots to refresh from one batch of event candidates.
#[derive(Default)]
struct RefreshCandidates {
    /// Roots known to need a refresh.
    matched_roots: HashSet<PathBuf>,
    /// Roots that need a refresh only if a candidate under them is a directory.
    pending_roots: Vec<PathBuf>,
    /// Distinct candidates under pending roots.
    to_stat: Vec<PathBuf>,
}

/// Add to MATCHED each pending root that contains a directory in TO_STAT.
/// Each candidate is stat'ed at most once, and not at all once every root
/// containing it has matched.
fn match_new_directories(
    matched: &mut HashSet<PathBuf>,
    pending_roots: &[PathBuf],
    to_stat: Vec<PathBuf>,
) {
    for candidate in to_stat {
        let unmatched: Vec<&PathBuf> = pending_roots
            .iter()
            .filter(|root| !matched.contains(*root) && candidate.starts_with(root))
            .collect();
        if !unmatched.is_empty() && candidate.is_dir() {
            matched.extend(unmatched.into_iter().cloned());
        }
    }
}

#[cfg(test)]
thread_local! {
    /// Test hook run between the recursive walk and publishing its watches.
    static AFTER_RECURSIVE_WALK: std::cell::RefCell<Option<Box<dyn FnOnce()>>> =
        std::cell::RefCell::new(None);
}

#[cfg(test)]
fn set_after_recursive_walk_hook(hook: impl FnOnce() + 'static) {
    AFTER_RECURSIVE_WALK.with(|slot| *slot.borrow_mut() = Some(Box::new(hook)));
}

#[cfg(test)]
fn run_after_recursive_walk_hook() {
    if let Some(hook) = AFTER_RECURSIVE_WALK.with(|slot| slot.borrow_mut().take()) {
        hook();
    }
}

/// Returns true if a `notify::Error` represents a transient missing path
/// (e.g. a temporary subdirectory deleted between scan and `add_watch`).
fn is_not_found_error(err: &notify::Error) -> bool {
    matches!(err.kind, notify::ErrorKind::PathNotFound)
        || matches!(
            &err.kind,
            notify::ErrorKind::Io(io_err) if io_err.kind() == std::io::ErrorKind::NotFound
        )
}

/// .gitignore-aware wrapper around `RecommendedWatcher`.
///
/// Recursive watches are registered as per-directory non-recursive watches.
/// Logical ownership (`recursive_roots` and `direct_watches`) is the single
/// source of truth for watched paths under one mutex.
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

    fn watched_mode(&self, path: &Path) -> Option<RecursiveMode> {
        if self.recursive_roots.contains_key(path) {
            Some(RecursiveMode::Recursive)
        } else if self.direct_watches.contains(path) {
            Some(RecursiveMode::NonRecursive)
        } else {
            None
        }
    }

    fn is_watched(&self, path: &Path) -> bool {
        self.watched_mode(path).is_some()
    }

    #[cfg(test)]
    fn list(&self) -> Vec<(PathBuf, bool)> {
        let mut items: Vec<(PathBuf, bool)> = self
            .recursive_roots
            .keys()
            .map(|p| (p.clone(), true))
            .collect();
        for p in &self.direct_watches {
            if !self.recursive_roots.contains_key(p) {
                items.push((p.clone(), false));
            }
        }
        items
    }

    fn all_watched_roots(&self) -> HashSet<PathBuf> {
        let mut all: HashSet<PathBuf> = self.recursive_roots.keys().cloned().collect();
        all.extend(self.direct_watches.iter().cloned());
        all
    }

    #[cfg(test)]
    fn watch(&mut self, path: &Path, mode: RecursiveMode) -> Result<(), notify::Error> {
        match mode {
            RecursiveMode::NonRecursive => self.watch_nonrecursive(path),
            RecursiveMode::Recursive => {
                let dirs = Self::collect_recursive_dirs(path);
                self.watch_recursive_with_dirs(path, dirs)
            }
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

    fn watch_recursive_with_dirs(
        &mut self,
        path: &Path,
        dirs: HashSet<PathBuf>,
    ) -> Result<(), notify::Error> {
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

    /// Atomically upgrade a non-recursive direct watch to recursive under one lock.
    fn upgrade_to_recursive(
        &mut self,
        canonical: &Path,
        dirs: HashSet<PathBuf>,
    ) -> Result<(), notify::Error> {
        if let Err(err) = self.unwatch(canonical) {
            // notify drops its own backend registration before the unwatch
            // syscall reports failure, so an error here can still leave the
            // path unwatched.  Restore a consistent non-recursive registration
            // before surfacing the error, otherwise direct_watches claims a
            // live watch that is gone.
            if self.rearm_existing_watch(canonical).is_err() {
                self.direct_watches.remove(canonical);
                self.remove_path_watch_best_effort(canonical);
            }
            return Err(err);
        }
        if let Err(err) = self.watch_recursive_with_dirs(canonical, dirs) {
            let _ = self.watch_nonrecursive(canonical);
            return Err(err);
        }
        Ok(())
    }

    fn recursive_roots(&self) -> Vec<PathBuf> {
        self.recursive_roots.keys().cloned().collect()
    }

    fn has_recursive_roots(&self) -> bool {
        !self.recursive_roots.is_empty()
    }

    /// Classify event candidate paths for recursive refresh.
    ///
    /// Roots containing a known directory among the candidates match at once,
    /// so deletes still refresh them.  The other roots containing candidates
    /// are returned as pending, with the distinct candidates under them; the
    /// caller stats each candidate once off-lock and matches only directories
    /// to their pending roots.
    fn classify_refresh_candidates(&self, candidates: &HashSet<PathBuf>) -> RefreshCandidates {
        let mut matched_roots = HashSet::new();
        let mut pending_roots = Vec::new();
        for (root, dirs) in &self.recursive_roots {
            if candidates.iter().any(|path| dirs.contains(path)) {
                matched_roots.insert(root.clone());
            } else if candidates.iter().any(|path| path.starts_with(root)) {
                pending_roots.push(root.clone());
            }
        }
        let to_stat = candidates
            .iter()
            .filter(|path| pending_roots.iter().any(|root| path.starts_with(root)))
            .cloned()
            .collect();
        RefreshCandidates {
            matched_roots,
            pending_roots,
            to_stat,
        }
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
        self.apply_recursive_dirs_inner(root, next, false)
    }

    /// Best-effort refresh variant: skips transient `NotFound` subdirectories
    /// that disappeared between scan and `add_watch` instead of rolling back
    /// the entire root's newly discovered watches.
    fn refresh_recursive_dirs_best_effort(&mut self, root: &Path, next: HashSet<PathBuf>) {
        let _ = self.apply_recursive_dirs_inner(root, next, true);
    }

    fn apply_recursive_dirs_inner(
        &mut self,
        root: &Path,
        next: HashSet<PathBuf>,
        skip_transient_missing: bool,
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
            match self.add_path_watch(dir) {
                Ok(()) => {
                    added.push(dir.clone());
                    applied.insert(dir.clone());
                }
                Err(err) if skip_transient_missing && dir != root && is_not_found_error(&err) => {
                    // Subdirectory vanished between scan and add_watch; keep
                    // applying the rest of the refresh instead of rolling back.
                    continue;
                }
                Err(err) => {
                    for added_dir in &added {
                        self.remove_path_watch_best_effort(added_dir);
                        applied.remove(added_dir);
                    }
                    self.recursive_roots.insert(root.to_path_buf(), applied);
                    return Err(err);
                }
            }
        }

        self.recursive_roots.insert(root.to_path_buf(), applied);
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

#[derive(Debug)]
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
use tokio::io::unix::AsyncFd;

#[cfg(any(target_os = "linux", target_os = "android"))]
enum NofollowReader {
    Async {
        stop: Arc<Notify>,
        task: tokio::task::JoinHandle<()>,
    },
    Thread {
        running: Arc<AtomicBool>,
        handle: Option<std::thread::JoinHandle<()>>,
    },
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn canonicalize_parent_preserve_leaf(path: &Path) -> PathBuf {
    match (path.parent(), path.file_name()) {
        (Some(parent), Some(leaf)) if !parent.as_os_str().is_empty() => {
            if let Ok(canonical_parent) = parent.canonicalize() {
                canonical_parent.join(leaf)
            } else {
                path.to_path_buf()
            }
        }
        _ => path.to_path_buf(),
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
struct NofollowSymlinkWatcher {
    fd: Arc<OwnedFd>,
    watches: HashMap<PathBuf, SymlinkWatchEntry>,
    /// Multiple distinct paths (e.g. hard-linked symlinks) can map to the
    /// same kernel watch descriptor.  Retain every watched path per `wd` so
    /// unwatching one path never silences another sharing the same `wd`.
    wd_to_paths: Arc<Mutex<HashMap<i32, HashSet<PathBuf>>>>,
    /// `(wd, path)` pairs removed by kernel `IN_IGNORED`; storing the path
    /// prevents a late purge from removing a newer watch that reused `wd`.
    ignored_entries: Arc<Mutex<Vec<(i32, PathBuf)>>>,
    /// Registered spellings that differ from their normalized watch path,
    /// with the number of registrations under that spelling.  Removal by such
    /// a spelling releases the watch it registered, even if a parent symlink
    /// was retargeted since.
    aliases: HashMap<PathBuf, (PathBuf, usize)>,
    reader: NofollowReader,
}

#[cfg(any(target_os = "linux", target_os = "android"))]
#[derive(Debug, Clone, Copy)]
struct SymlinkWatchEntry {
    wd: i32,
    count: usize,
}

#[cfg(any(target_os = "linux", target_os = "android"))]
impl NofollowSymlinkWatcher {
    fn new(
        tx: WatchInputSender,
        runtime: Option<&tokio::runtime::Handle>,
    ) -> Result<Self, notify::Error> {
        let raw_fd = inotify::init(inotify::CreateFlags::NONBLOCK | inotify::CreateFlags::CLOEXEC)
            .map_err(|err| notify::Error::io(err.into()))?;

        let wd_to_paths = Arc::new(Mutex::new(HashMap::new()));
        let ignored_entries = Arc::new(Mutex::new(Vec::new()));

        let active_handle = runtime
            .cloned()
            .or_else(|| tokio::runtime::Handle::try_current().ok());

        if let Some(handle) = active_handle {
            let _enter = handle.enter();
            let fd = Arc::new(raw_fd);
            // Duplicate for AsyncFd registration so `self.fd` stays an
            // independent `OwnedFd` while the async task drives epoll readiness
            // with zero polling.
            let dup = rustix::io::fcntl_dupfd_cloexec(&*fd, 0)
                .map_err(|err| notify::Error::io(err.into()))?;
            let async_fd = AsyncFd::new(dup).map_err(notify::Error::io)?;
            let stop = Arc::new(Notify::new());
            let task = spawn_nofollow_reader_async(
                &handle,
                async_fd,
                Arc::clone(&wd_to_paths),
                Arc::clone(&ignored_entries),
                Arc::clone(&stop),
                tx,
            );
            Ok(Self {
                fd,
                watches: HashMap::new(),
                wd_to_paths,
                ignored_entries,
                aliases: HashMap::new(),
                reader: NofollowReader::Async { stop, task },
            })
        } else {
            let fd = Arc::new(raw_fd);
            let running = Arc::new(AtomicBool::new(true));
            let handle = Some(spawn_nofollow_reader_thread(
                Arc::clone(&fd),
                Arc::clone(&wd_to_paths),
                Arc::clone(&ignored_entries),
                Arc::clone(&running),
                tx,
            ));
            Ok(Self {
                fd,
                watches: HashMap::new(),
                wd_to_paths,
                ignored_entries,
                aliases: HashMap::new(),
                reader: NofollowReader::Thread { running, handle },
            })
        }
    }

    fn contains(&mut self, path: &Path) -> bool {
        self.purge_ignored();
        self.watches.contains_key(path)
            || self.aliases.contains_key(path)
            || self
                .watches
                .contains_key(&canonicalize_parent_preserve_leaf(path))
    }

    /// Remember that SPELLING registered the watch at NORMALIZED.
    fn record_alias(&mut self, spelling: &Path, normalized: &Path) {
        if spelling != normalized {
            self.aliases
                .entry(spelling.to_path_buf())
                .or_insert_with(|| (normalized.to_path_buf(), 0))
                .1 += 1;
        }
    }

    /// Resolve PATH to the normalized watch key it refers to, consuming one
    /// alias registration when PATH is a recorded spelling.
    fn take_watch_key(&mut self, path: &Path) -> Option<PathBuf> {
        if let Some((normalized, count)) = self.aliases.get_mut(path) {
            let normalized = normalized.clone();
            *count -= 1;
            if *count == 0 {
                self.aliases.remove(path);
            }
            return self.watches.contains_key(&normalized).then_some(normalized);
        }
        if self.watches.contains_key(path) {
            return Some(path.to_path_buf());
        }
        let normalized = canonicalize_parent_preserve_leaf(path);
        self.watches.contains_key(&normalized).then_some(normalized)
    }

    fn watch(&mut self, path: &Path) -> Result<PathBuf, notify::Error> {
        self.purge_ignored();
        // Canonicalize parent directories while preserving the leaf symlink so
        // different parent spellings collapse to one watched path.
        let spelling = path;
        let path = canonicalize_parent_preserve_leaf(path);
        if let Some(entry) = self.watches.get_mut(&path) {
            entry.count += 1;
            self.record_alias(spelling, &path);
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
        // cleanup, but do expect `set-file-times` with nofollow to report an
        // attribute change.
        let mask = inotify::WatchFlags::ATTRIB | inotify::WatchFlags::DONT_FOLLOW;
        let wd = inotify::add_watch(&*self.fd, &path, mask)
            .map_err(|err| notify::Error::io(err.into()).add_path(path.clone()))?;

        self.watches
            .insert(path.clone(), SymlinkWatchEntry { wd, count: 1 });
        lock_or_recover(&self.wd_to_paths)
            .entry(wd)
            .or_default()
            .insert(path.clone());
        self.record_alias(spelling, &path);
        Ok(path)
    }

    fn unwatch(&mut self, path: &Path) -> Result<(), notify::Error> {
        self.purge_ignored();
        let Some(key) = self.take_watch_key(path) else {
            return Err(notify::Error::watch_not_found().add_path(path.to_path_buf()));
        };

        let entry = self.watches.get_mut(&key).expect("key verified above");
        if entry.count > 1 {
            entry.count -= 1;
            return Ok(());
        }

        let wd = entry.wd;
        self.watches.remove(&key);
        self.aliases.retain(|_, (normalized, _)| *normalized != key);

        let remove_kernel_watch = {
            let mut wd_to_paths = lock_or_recover(&self.wd_to_paths);
            if let Some(paths) = wd_to_paths.get_mut(&wd) {
                paths.remove(&key);
                if paths.is_empty() {
                    wd_to_paths.remove(&wd);
                    true
                } else {
                    false
                }
            } else {
                true
            }
        };

        if remove_kernel_watch {
            match inotify::remove_watch(&*self.fd, wd) {
                Ok(()) => {}
                // EINVAL means the kernel already auto-removed the watch (e.g.
                // symlink was deleted) before IN_IGNORED was drained; clean up
                // our local maps instead of leaking the stale entry.
                Err(rustix::io::Errno::INVAL) => {}
                Err(err) => {
                    return Err(notify::Error::io(err.into()).add_path(key));
                }
            }
        }
        Ok(())
    }

    fn purge_ignored(&mut self) {
        let ignored: Vec<_> = lock_or_recover(&self.ignored_entries).drain(..).collect();
        if ignored.is_empty() {
            return;
        }

        for (wd, path) in ignored {
            if self.watches.get(&path).is_some_and(|entry| entry.wd == wd) {
                self.watches.remove(&path);
                self.aliases
                    .retain(|_, (normalized, _)| *normalized != path);
            }
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
impl Drop for NofollowSymlinkWatcher {
    fn drop(&mut self) {
        match &mut self.reader {
            NofollowReader::Async { stop, task } => {
                stop.notify_one();
                task.abort();
            }
            NofollowReader::Thread { running, handle } => {
                running.store(false, Ordering::Relaxed);
                if let Some(handle) = handle.take() {
                    let _ = handle.join();
                }
            }
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn spawn_nofollow_reader_async(
    handle: &tokio::runtime::Handle,
    async_fd: AsyncFd<OwnedFd>,
    wd_to_paths: Arc<Mutex<HashMap<i32, HashSet<PathBuf>>>>,
    ignored_entries: Arc<Mutex<Vec<(i32, PathBuf)>>>,
    stop: Arc<Notify>,
    tx: WatchInputSender,
) -> tokio::task::JoinHandle<()> {
    handle.spawn(async move {
        let mut buf = [std::mem::MaybeUninit::<u8>::uninit(); 4096];
        loop {
            let mut guard = tokio::select! {
                () = stop.notified() => break,
                ready = async_fd.readable() => match ready {
                    Ok(guard) => guard,
                    Err(_) => break,
                },
            };
            let mut fatal = false;
            let mut events = inotify::Reader::new(async_fd.get_ref(), &mut buf);
            loop {
                match events.next() {
                    Ok(event) => emit_nofollow_event(&event, &wd_to_paths, &ignored_entries, &tx),
                    Err(rustix::io::Errno::AGAIN) => {
                        guard.clear_ready();
                        break;
                    }
                    Err(rustix::io::Errno::INTR) => continue,
                    Err(_) => {
                        fatal = true;
                        break;
                    }
                }
            }
            if fatal {
                break;
            }
        }
    })
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn spawn_nofollow_reader_thread(
    fd: Arc<OwnedFd>,
    wd_to_paths: Arc<Mutex<HashMap<i32, HashSet<PathBuf>>>>,
    ignored_entries: Arc<Mutex<Vec<(i32, PathBuf)>>>,
    running: Arc<AtomicBool>,
    tx: WatchInputSender,
) -> std::thread::JoinHandle<()> {
    std::thread::spawn(move || {
        let mut buf = [std::mem::MaybeUninit::<u8>::uninit(); 4096];
        let mut events = inotify::Reader::new(&*fd, &mut buf);
        while running.load(Ordering::Relaxed) {
            let drained = loop {
                match events.next() {
                    Ok(event) => emit_nofollow_event(&event, &wd_to_paths, &ignored_entries, &tx),
                    Err(rustix::io::Errno::AGAIN) | Err(rustix::io::Errno::INTR) => break true,
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
fn handle_nofollow_raw_event(
    wd: i32,
    flags: inotify::ReadFlags,
    wd_to_paths: &Mutex<HashMap<i32, HashSet<PathBuf>>>,
    ignored_entries: &Mutex<Vec<(i32, PathBuf)>>,
    tx: &WatchInputSender,
) {
    // Kernel queue overflow arrives with wd == -1 and no matching watch entry.
    // Handle it before looking up `wd` so consumers receive a rescan signal.
    if flags.contains(inotify::ReadFlags::QUEUE_OVERFLOW) {
        tx.send(WatchInput::Direct(vec![WatchEvent::rescan()]));
        return;
    }

    let mut map = lock_or_recover(wd_to_paths);
    let paths: Vec<PathBuf> = map
        .get(&wd)
        .map(|set| set.iter().cloned().collect())
        .unwrap_or_default();
    if flags.contains(inotify::ReadFlags::IGNORED)
        && let Some(removed_paths) = map.remove(&wd)
    {
        let mut ignored = lock_or_recover(ignored_entries);
        for removed_path in removed_paths {
            ignored.push((wd, removed_path));
        }
    }
    drop(map);

    if flags.contains(inotify::ReadFlags::ATTRIB) && !paths.is_empty() {
        let events = paths
            .into_iter()
            .map(|path| WatchEvent::path("attribute-changed", path))
            .collect();
        tx.send(WatchInput::Direct(events));
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn emit_nofollow_event(
    event: &inotify::Event<'_>,
    wd_to_paths: &Mutex<HashMap<i32, HashSet<PathBuf>>>,
    ignored_entries: &Mutex<Vec<(i32, PathBuf)>>,
    tx: &WatchInputSender,
) {
    handle_nofollow_raw_event(event.wd(), event.events(), wd_to_paths, ignored_entries, tx);
}

#[cfg(not(any(target_os = "linux", target_os = "android")))]
struct NofollowSymlinkWatcher;

#[cfg(not(any(target_os = "linux", target_os = "android")))]
impl NofollowSymlinkWatcher {
    fn new(
        _tx: WatchInputSender,
        _runtime: Option<&tokio::runtime::Handle>,
    ) -> Result<Self, notify::Error> {
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
    /// The underlying OS watcher (inotify/kqueue) and its single-source-of-truth
    /// watched-path maps.  Protected by `std::sync::Mutex` because `notify`'s
    /// callback runs on its own thread, not a Tokio thread.
    watcher: Mutex<FilteredWatcher>,

    /// Lazily initialized nofollow symlink watcher (created on first nofollow
    /// watch so idle servers spend zero threads/fds on unused symlink watches).
    symlink_watcher: Mutex<Option<NofollowSymlinkWatcher>>,

    /// Channel sender cloned into `NofollowSymlinkWatcher` upon lazy init.
    input: WatchInputSender,

    /// Tokio runtime handle captured at startup for spawning the async
    /// `NofollowSymlinkWatcher` task from `spawn_blocking` contexts.
    runtime: Option<tokio::runtime::Handle>,
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
            symlink_watcher: Mutex::new(None),
            input,
            runtime: tokio::runtime::Handle::try_current().ok(),
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
            let mut slot = lock_or_recover(&self.symlink_watcher);
            let watcher = match slot.as_mut() {
                Some(w) => w,
                None => {
                    let created =
                        NofollowSymlinkWatcher::new(self.input.clone(), self.runtime.as_ref())?;
                    slot.insert(created)
                }
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

        // Fast-path check under lock before doing an expensive tree walk.
        {
            let watcher = lock_or_recover(&self.watcher);
            match watcher.watched_mode(&canonical) {
                Some(existing) if existing == mode => return Ok(canonical),
                Some(RecursiveMode::Recursive) => return Ok(canonical),
                _ => {}
            }
        }

        // Perform any recursive directory walk OUTSIDE the watcher mutex so
        // concurrent watch/unwatch/debounce operations are not blocked.
        let scanned_dirs = if recursive {
            Some(FilteredWatcher::collect_recursive_dirs(&canonical))
        } else {
            None
        };
        #[cfg(test)]
        if recursive {
            run_after_recursive_walk_hook();
        }

        let mut watcher = lock_or_recover(&self.watcher);
        match watcher.watched_mode(&canonical) {
            Some(existing) if existing == mode => return Ok(canonical),
            Some(RecursiveMode::Recursive) => return Ok(canonical),
            Some(RecursiveMode::NonRecursive) => {
                let dirs = scanned_dirs.expect("recursive upgrade must have scanned dirs");
                watcher.upgrade_to_recursive(&canonical, dirs)?;
            }
            None => match scanned_dirs {
                Some(dirs) => watcher.watch_recursive_with_dirs(&canonical, dirs)?,
                None => watcher.watch_nonrecursive(&canonical)?,
            },
        }
        drop(watcher);

        if recursive {
            // A directory created after the walk passed its parent, but
            // before that parent's watch was published, produced no event
            // here (and another root's refresh may have claimed it).  Every
            // published directory is watched now, so a second walk finds
            // all directories created before this point; later ones raise
            // events.
            let dirs = FilteredWatcher::collect_recursive_dirs(&canonical);
            lock_or_recover(&self.watcher).refresh_recursive_dirs_best_effort(&canonical, dirs);
        }

        Ok(canonical)
    }

    /// Stop watching a path.
    ///
    /// Looks up the stored canonical path from when watch() was called,
    /// so this works even if the directory has been deleted since then.
    #[cfg(test)]
    pub fn unwatch(&self, path: &Path) -> Result<(), notify::Error> {
        self.unwatch_with_options(path, None)
    }

    /// Remove exactly the requested backend.  None preserves legacy removal
    /// of both registrations; a symlink and its target have independent owners.
    pub fn unwatch_with_options(
        &self,
        path: &Path,
        nofollow: Option<bool>,
    ) -> Result<(), notify::Error> {
        let mut removed_nofollow = false;
        if nofollow != Some(false) {
            let mut symlink_watcher = lock_or_recover(&self.symlink_watcher);
            if let Some(watcher) = symlink_watcher.as_mut()
                && watcher.contains(path)
            {
                watcher.unwatch(path)?;
                removed_nofollow = true;
            }
        }

        if nofollow == Some(true) {
            return if removed_nofollow {
                Ok(())
            } else {
                Err(notify::Error::generic("Nofollow path not being watched"))
            };
        }

        // Try to canonicalize, but fall back to the raw path
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

        let mut watcher = lock_or_recover(&self.watcher);
        if !watcher.is_watched(&canonical) {
            if removed_nofollow {
                return Ok(());
            }
            return Err(notify::Error::generic(&format!(
                "Path not being watched (canonical: {}): {}",
                canonical.display(),
                path.display()
            )));
        }

        watcher.unwatch(&canonical)?;
        Ok(())
    }

    /// List currently watched paths and whether they are recursive.
    #[cfg(test)]
    pub fn list(&self) -> Vec<(PathBuf, bool)> {
        lock_or_recover(&self.watcher).list()
    }

    fn overflow_recovery_paths(&self) -> (HashSet<PathBuf>, HashSet<PathBuf>) {
        let watcher = lock_or_recover(&self.watcher);
        let recursive_roots = watcher.recursive_roots().into_iter().collect();
        let watched_paths = watcher.all_watched_roots();
        (recursive_roots, watched_paths)
    }

    /// Resolve which recursive roots need refreshing from accumulated event
    /// candidates without doing any `stat` calls on Tokio workers or under lock.
    async fn resolve_and_refresh_recursive_roots(
        &self,
        need_rescan_all: bool,
        refresh_candidates: HashSet<PathBuf>,
        ignore_paths: HashSet<PathBuf>,
    ) {
        let RefreshCandidates {
            matched_roots: mut roots_to_refresh,
            pending_roots,
            to_stat,
        } = {
            let watcher = lock_or_recover(&self.watcher);
            if !watcher.has_recursive_roots() {
                return;
            }
            if need_rescan_all {
                RefreshCandidates {
                    matched_roots: watcher.recursive_roots().into_iter().collect(),
                    ..RefreshCandidates::default()
                }
            } else {
                let mut classified = if refresh_candidates.is_empty() {
                    RefreshCandidates::default()
                } else {
                    watcher.classify_refresh_candidates(&refresh_candidates)
                };
                for path in &ignore_paths {
                    classified
                        .matched_roots
                        .extend(watcher.recursive_roots_for_ignore_rule(path));
                }
                classified
            }
        };

        if roots_to_refresh.is_empty() && to_stat.is_empty() {
            return;
        }

        // Run both directory liveness checks (`is_dir`) and tree scans off the
        // Tokio worker threads.
        let scanned = tokio::task::spawn_blocking(move || {
            match_new_directories(&mut roots_to_refresh, &pending_roots, to_stat);
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

        if scanned.is_empty() {
            return;
        }

        let mut watcher = lock_or_recover(&self.watcher);
        for (root, dirs) in scanned {
            watcher.refresh_recursive_dirs_best_effort(&root, dirs);
        }
    }

    /// Rebind still-existing watches after Linux/inotify inode replacement.
    ///
    /// Must run after `resolve_and_refresh_recursive_roots`, so genuine
    /// deletes are already pruned and only path-identical replacements remain.
    async fn rearm_suspect_paths(&self, suspect_paths: HashSet<PathBuf>) {
        if suspect_paths.is_empty() {
            return;
        }
        let suspects: Vec<PathBuf> = suspect_paths.into_iter().collect();
        let candidates = {
            let watcher = lock_or_recover(&self.watcher);
            watcher.watched_paths_under(&suspects)
        };
        if candidates.is_empty() {
            return;
        }
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

/// Candidate paths that might require refreshing a recursive root's directory
/// watch set.  Does NOT call `path.is_dir()` synchronously; directory checks
/// happen inside `spawn_blocking` after the debounce window closes.
fn directory_tree_refresh_candidates(event: &Event) -> &[PathBuf] {
    match event.kind {
        EventKind::Create(_)
        | EventKind::Modify(ModifyKind::Any | ModifyKind::Other | ModifyKind::Name(_))
        | EventKind::Remove(RemoveKind::Any | RemoveKind::Folder | RemoveKind::Other) => {
            &event.paths
        }
        _ => &[],
    }
}

/// Linux/inotify inputs for [`WatchManager::rearm_suspect_paths`].
#[cfg(target_os = "linux")]
fn inode_replacing_paths(event: &Event) -> &[PathBuf] {
    match event.kind {
        EventKind::Remove(_) | EventKind::Modify(ModifyKind::Name(_)) => &event.paths,
        _ => &[],
    }
}

#[cfg(not(target_os = "linux"))]
fn inode_replacing_paths(_event: &Event) -> &[PathBuf] {
    &[]
}

fn ignore_rule_paths(event: &Event) -> impl Iterator<Item = &PathBuf> {
    let relevant = matches!(
        event.kind,
        EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_)
    );
    event
        .paths
        .iter()
        .filter(move |path| relevant && is_ignore_rule_path(path))
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

#[derive(Default)]
struct PendingBatch {
    events: Vec<WatchEvent>,
    refresh_candidates: HashSet<PathBuf>,
    ignore_paths: HashSet<PathBuf>,
    suspect_paths: HashSet<PathBuf>,
    need_rescan_all: bool,
}

/// Background task: receives raw inotify events, debounces them, and sends
/// batched `fs.events` notifications to the Emacs client.
///
/// Algorithm (fixed-window debounce):
/// 1. Wait for the first event (blocks until something happens)
/// 2. Start a 200ms timer
/// 3. Collect all events that arrive during the timer window (no blocking IO)
/// 4. When the timer fires, refresh recursive watches off-thread & send one
///    notification with all collected events
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

        let mut batch = PendingBatch::default();
        let mut batch_overflowed = event.is_some_and(|event| collect_input(event, &mut batch));

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
                                batch_overflowed = collect_input(e, &mut batch);
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
                batch.need_rescan_all = true;
                let (_, all_watched_paths) = manager.overflow_recovery_paths();
                batch.suspect_paths.extend(all_watched_paths);
            }
            manager
                .resolve_and_refresh_recursive_roots(
                    batch.need_rescan_all,
                    batch.refresh_candidates,
                    batch.ignore_paths,
                )
                .await;
            manager.rearm_suspect_paths(batch.suspect_paths).await;
        }
        if overflowed {
            coalesce_to_rescan(&mut batch.events);
        }

        // Phase 3: Send notification with all collected events.
        // Only terminate the watcher loop on a broken transport (stdout EOF /
        // EPIPE).  Encoding / oversized single-event errors degrade to a
        // rescan notification instead of permanently killing file notifications.
        if !batch.events.is_empty() {
            match send_notification(&writer, &batch.events).await {
                Ok(()) => {}
                Err(SendNotificationError::Transport) => break,
                Err(SendNotificationError::Encode) => {
                    let rescan = [WatchEvent::rescan()];
                    if matches!(
                        send_notification(&writer, &rescan).await,
                        Err(SendNotificationError::Transport)
                    ) {
                        break;
                    }
                }
            }
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

fn collect_input(input: WatchInput, batch: &mut PendingBatch) -> bool {
    match input {
        WatchInput::Notify(event) => {
            let rescan_required = event.need_rescan();
            if rescan_required {
                batch.need_rescan_all = true;
            } else {
                batch
                    .refresh_candidates
                    .extend(directory_tree_refresh_candidates(&event).iter().cloned());
                batch
                    .ignore_paths
                    .extend(ignore_rule_paths(&event).cloned());
            }

            batch
                .suspect_paths
                .extend(inode_replacing_paths(&event).iter().cloned());
            append_watch_events(&mut batch.events, event_to_watch_events(&event)) || rescan_required
        }
        #[cfg(any(target_os = "linux", target_os = "android"))]
        WatchInput::Direct(events) => append_watch_events(&mut batch.events, events),
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

#[derive(Debug)]
enum SendNotificationError {
    Encode,
    Transport,
}

async fn send_notification_with_limit<W>(
    writer: &Arc<crate::output::FrameWriter<W>>,
    events: &[WatchEvent],
    max_frame_size: usize,
) -> Result<(), SendNotificationError>
where
    W: tokio::io::AsyncWrite + Unpin,
{
    let mut batches = vec![events];
    while let Some(batch) = batches.pop() {
        if batch.is_empty() {
            continue;
        }
        let framed = encode_framed(&fs_events_notification(batch))
            .map_err(|_| SendNotificationError::Encode)?;
        let payload_len = framed.len().saturating_sub(4);
        if payload_len > max_frame_size {
            if batch.len() == 1 {
                return Err(SendNotificationError::Encode);
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
        writer
            .write_prefixed_frame(&framed)
            .await
            .map_err(|_| SendNotificationError::Transport)?;
        tokio::task::yield_now().await;
    }
    Ok(())
}

/// Serialize and send bounded `fs.events` notifications over the stdout writer.
/// Oversized debounce batches are split and streamed in event order.
async fn send_notification(
    writer: &WriterHandle,
    events: &[WatchEvent],
) -> Result<(), SendNotificationError> {
    send_notification_with_limit(writer, events, crate::MAX_FRAME_SIZE).await
}

// ============================================================================
// RPC handlers for watch.add, watch.remove
// ============================================================================

use crate::handlers::{HandlerResult, run_blocking};

/// Handle `watch.add` - start watching a directory or symlink for changes.
///
/// Params: `{ "path": "/path/to/dir", "recursive": true|false,
///            "nofollow": true|false, "nofollow_if_symlink": true|false }`
///
/// Returns `{ "path", "recursive", "nofollow", "is_symlink", "canonical_path" }`
/// so Emacs `file-notify-add-watch` needs only 1 RTT instead of 3.
pub async fn handle_add(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: RpcPath,
        #[serde(default = "default_recursive")]
        recursive: bool,
        #[serde(default)]
        nofollow: bool,
        #[serde(default)]
        nofollow_if_symlink: bool,
    }
    fn default_recursive() -> bool {
        true
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    // Run tilde expansion, symlink probe, canonicalization, and any recursive
    // directory tree walk in a single blocking task off Tokio workers.
    let manager = Arc::clone(manager);
    let rpc_path = params.path;
    let recursive = params.recursive;
    let requested_nofollow = params.nofollow;
    let nofollow_if_symlink = params.nofollow_if_symlink;

    let (watched, effective_nofollow, is_symlink, canonical_path) =
        run_blocking("Watch add", move || {
            let path = rpc_path.resolve_sync();
            let is_symlink = std::fs::symlink_metadata(&path)
                .map(|m| m.file_type().is_symlink())
                .unwrap_or(false);
            let effective_nofollow = requested_nofollow || (nofollow_if_symlink && is_symlink);
            let watched = if effective_nofollow {
                manager.watch_with_options(&path, recursive, true)
            } else {
                manager.watch(&path, recursive)
            }
            .map_err(|e| RpcError::internal_error(format!("Failed to watch: {e}")))?;
            let canonical_path = if effective_nofollow {
                path.canonicalize().ok()
            } else {
                Some(watched.clone())
            };
            Ok((watched, effective_nofollow, is_symlink, canonical_path))
        })
        .await?;

    Ok(msgpack_map! {
        "path" => path_to_value(&watched),
        "recursive" => Value::Boolean(recursive),
        "nofollow" => Value::Boolean(effective_nofollow),
        "is_symlink" => Value::Boolean(is_symlink),
        "canonical_path" => canonical_path.as_deref().map_or(Value::Nil, path_to_value)
    })
}

/// Handle `watch.remove` - stop watching a directory or symlink.
///
/// Params: `{ "path": "/path/to/dir", "nofollow": true|false }`
/// Omitting `nofollow` retains removal of both registrations.
pub async fn handle_remove(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: RpcPath,
        #[serde(default)]
        nofollow: Option<bool>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let manager = get().ok_or_else(|| RpcError::internal_error("File watcher not available"))?;

    let manager = Arc::clone(manager);
    let rpc_path = params.path;
    let nofollow = params.nofollow;
    run_blocking("Watch remove", move || {
        let path = rpc_path.resolve_sync();
        manager
            .unwatch_with_options(&path, nofollow)
            .map_err(|e| RpcError::internal_error(format!("Failed to unwatch: {e}")))
    })
    .await?;

    Ok(Value::Boolean(true))
}

#[cfg(test)]
mod tests;
