// SPDX-License-Identifier: GPL-3.0-or-later

//! Tests for the filesystem watcher.

use super::*;
use notify::event::{CreateKind, Flag};
use std::fs;
#[cfg(target_os = "linux")]
use std::sync::mpsc as std_mpsc;
#[cfg(target_os = "linux")]
use std::time::{Duration, Instant};

fn test_input_channel(capacity: usize) -> (WatchInputSender, mpsc::Receiver<WatchInput>) {
    let (tx, rx) = mpsc::channel(capacity);
    (
        WatchInputSender {
            tx,
            overflowed: Arc::new(AtomicBool::new(false)),
            overflow_notify: Arc::new(Notify::new()),
        },
        rx,
    )
}

fn test_manager() -> WatchManager {
    let (input, _rx) = test_input_channel(1);
    WatchManager {
        watcher: Mutex::new(FilteredWatcher::new(|_: notify::Result<Event>| {}).unwrap()),
        watched_paths: Mutex::new(HashMap::new()),
        symlink_watcher: Mutex::new(NofollowSymlinkWatcher::new(input).ok()),
    }
}

fn map_value<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
    match value {
        Value::Map(fields) => fields.iter().find_map(|(k, v)| match k {
            Value::String(s) if s.as_str() == Some(key) => Some(v),
            _ => None,
        }),
        _ => None,
    }
}

#[tokio::test]
async fn test_watch_input_overflow_sets_flag_and_wakes_receiver() {
    let (input, _rx) = test_input_channel(1);
    input.send(WatchInput::Notify(Event::new(EventKind::Any)));
    input.send(WatchInput::Notify(Event::new(EventKind::Any)));
    assert!(input.overflowed.load(Ordering::Acquire));
    tokio::time::timeout(
        std::time::Duration::from_secs(1),
        input.overflow_notify.notified(),
    )
    .await
    .expect("overflow must wake the debounce task");
}

#[test]
fn test_debounce_event_batch_is_bounded() {
    let mut pending = Vec::new();
    let events = (0..=MAX_DEBOUNCE_EVENTS)
        .map(|index| WatchEvent::path("changed", PathBuf::from(index.to_string())))
        .collect();
    assert!(append_watch_events(&mut pending, events));
    assert_eq!(pending.len(), MAX_DEBOUNCE_EVENTS);
    coalesce_to_rescan(&mut pending);
    assert_eq!(pending, vec![WatchEvent::rescan()]);
}

#[test]
fn test_overflow_recovery_includes_recursive_and_direct_watches() {
    let temp = tempfile::tempdir().unwrap();
    let recursive = temp.path().join("recursive");
    let direct = temp.path().join("direct");
    fs::create_dir_all(&recursive).unwrap();
    fs::create_dir_all(&direct).unwrap();
    let manager = test_manager();
    let recursive = manager.watch(&recursive, true).unwrap();
    let direct = manager.watch(&direct, false).unwrap();

    let (refresh, rearm) = manager.overflow_recovery_paths();
    assert!(refresh.contains(&recursive));
    assert!(!refresh.contains(&direct));
    assert!(rearm.contains(&recursive));
    assert!(rearm.contains(&direct));
}

#[tokio::test]
async fn test_split_notification_frames_are_streamed_in_order() {
    use tokio::io::AsyncReadExt;

    let events = vec![
        WatchEvent::path("created", PathBuf::from("/tmp/first")),
        WatchEvent::path("deleted", PathBuf::from("/tmp/second")),
    ];
    let limit = rmp_serde::to_vec_named(&fs_events_notification(&events[..1]))
        .unwrap()
        .len()
        .max(
            rmp_serde::to_vec_named(&fs_events_notification(&events[1..]))
                .unwrap()
                .len(),
        );
    let (write, mut read) = tokio::io::duplex(4096);
    let writer = Arc::new(tokio::sync::Mutex::new(write));
    send_notification_with_limit(&writer, &events, limit)
        .await
        .unwrap();
    drop(writer);

    let mut bytes = Vec::new();
    read.read_to_end(&mut bytes).await.unwrap();
    let mut offset = 0;
    let mut frame_count = 0;
    let mut decoded = Vec::new();
    while offset < bytes.len() {
        let length = u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize;
        assert!(length <= limit);
        frame_count += 1;
        offset += 4;
        let notification: Value = rmp_serde::from_slice(&bytes[offset..offset + length]).unwrap();
        offset += length;
        let events = notification
            .as_map()
            .and_then(|notification| {
                notification
                    .iter()
                    .find(|(key, _)| key.as_str() == Some("params"))
            })
            .and_then(|(_, params)| params.as_map())
            .and_then(|params| {
                params
                    .iter()
                    .find(|(key, _)| key.as_str() == Some("events"))
            })
            .and_then(|(_, events)| events.as_array())
            .unwrap();
        decoded.extend(events.iter().filter_map(|event| {
            event
                .as_map()
                .and_then(|event| event.iter().find(|(key, _)| key.as_str() == Some("action")))
                .and_then(|(_, action)| action.as_str())
                .map(str::to_owned)
        }));
    }
    assert_eq!(frame_count, 2);
    assert_eq!(decoded, ["created", "deleted"]);
}

#[test]
fn test_watch_event_mapping_basic_actions() {
    let path = PathBuf::from("/tmp/file");

    assert_eq!(
        event_to_watch_events(
            &Event::new(EventKind::Create(CreateKind::File)).add_path(path.clone())
        ),
        vec![WatchEvent::path("created", path.clone())]
    );
    assert_eq!(
        event_to_watch_events(
            &Event::new(EventKind::Modify(ModifyKind::Data(DataChange::Content)))
                .add_path(path.clone())
        ),
        vec![WatchEvent::path("changed", path.clone())]
    );
    assert_eq!(
        event_to_watch_events(
            &Event::new(EventKind::Modify(ModifyKind::Metadata(
                MetadataKind::Permissions,
            )))
            .add_path(path.clone())
        ),
        vec![WatchEvent::path("attribute-changed", path.clone())]
    );
    assert_eq!(
        event_to_watch_events(
            &Event::new(EventKind::Remove(RemoveKind::File)).add_path(path.clone())
        ),
        vec![WatchEvent::path("deleted", path)]
    );
}

#[cfg(target_os = "linux")]
#[test]
fn test_nofollow_symlink_watch_reports_link_attribute_change() {
    let temp = tempfile::tempdir().unwrap();
    let target = temp.path().join("target");
    let link = temp.path().join("link");
    fs::write(&target, "target").unwrap();
    std::os::unix::fs::symlink(&target, &link).unwrap();

    let (input, mut rx) = test_input_channel(16);
    let mut watcher = NofollowSymlinkWatcher::new(input).unwrap();
    assert_eq!(watcher.watch(&link).unwrap(), link);

    fs::write(&target, "changed").unwrap();
    std::thread::sleep(Duration::from_millis(200));
    assert!(
        rx.try_recv().is_err(),
        "target changes must not be reported"
    );

    set_symlink_mtime(&link, 1);
    let deadline = Instant::now() + Duration::from_secs(2);
    let mut found = false;
    while Instant::now() < deadline {
        match rx.try_recv() {
            Ok(WatchInput::Direct(events)) => {
                found |= events.iter().any(|event| {
                    event.action == "attribute-changed" && event.path.as_ref() == Some(&link)
                });
                if found {
                    break;
                }
            }
            Ok(WatchInput::Notify(_)) => {}
            Err(_) => std::thread::sleep(Duration::from_millis(25)),
        }
    }

    assert!(found, "symlink metadata changes should be reported");
}

#[cfg(target_os = "linux")]
fn set_symlink_mtime(path: &Path, seconds: i64) {
    use rustix::fs::{AtFlags, CWD, Timespec, Timestamps};

    let times = Timestamps {
        last_access: Timespec {
            tv_sec: seconds,
            tv_nsec: 0,
        },
        last_modification: Timespec {
            tv_sec: seconds,
            tv_nsec: 0,
        },
    };
    rustix::fs::utimensat(CWD, path, &times, AtFlags::SYMLINK_NOFOLLOW)
        .expect("utimensat should succeed");
}

#[test]
fn test_watch_event_mapping_rename_actions() {
    let old = PathBuf::from("/tmp/old");
    let new = PathBuf::from("/tmp/new");

    assert_eq!(
        event_to_watch_events(
            &Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::Both)))
                .add_path(old.clone())
                .add_path(new.clone())
        ),
        vec![WatchEvent::rename(old.clone(), new)]
    );
    assert_eq!(
        event_to_watch_events(
            &Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::From)))
                .add_path(old.clone())
                .set_tracker(42)
        ),
        vec![WatchEvent::tracked("renamed-from", old.clone(), Some(42))]
    );
    assert_eq!(
        event_to_watch_events(
            &Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::To)))
                .add_path(old.clone())
                .set_tracker(42)
        ),
        vec![WatchEvent::tracked("renamed-to", old, Some(42))]
    );
}

#[test]
fn test_watch_event_serializes_paths_as_binary() {
    let event = WatchEvent::rename(PathBuf::from("/tmp/old"), PathBuf::from("/tmp/new"));
    let value = event.to_value();

    assert_eq!(
        map_value(&value, "action"),
        Some(&Value::String("renamed".into()))
    );
    assert_eq!(
        map_value(&value, "path"),
        Some(&Value::Binary(b"/tmp/old".to_vec()))
    );
    assert_eq!(
        map_value(&value, "path1"),
        Some(&Value::Binary(b"/tmp/new".to_vec()))
    );
}

#[test]
fn test_watch_event_mapping_includes_rescan() {
    let events = event_to_watch_events(&Event::new(EventKind::Any).set_flag(Flag::Rescan));

    assert_eq!(events, vec![WatchEvent::rescan()]);
}

#[test]
fn test_fs_events_notification_envelope() {
    let notification = fs_events_notification(&[
        WatchEvent::path("created", PathBuf::from("/tmp/new")),
        WatchEvent::rescan(),
    ]);

    assert_eq!(notification.version, "2.0");
    assert_eq!(notification.method, "fs.events");
    let events = match map_value(&notification.params, "events") {
        Some(Value::Array(events)) => events,
        other => panic!("expected events array, got {other:?}"),
    };
    assert_eq!(events.len(), 2);
    assert_eq!(
        map_value(&events[0], "action"),
        Some(&Value::String("created".into()))
    );
    assert_eq!(
        map_value(&events[0], "path"),
        Some(&Value::Binary(b"/tmp/new".to_vec()))
    );
    assert_eq!(
        map_value(&events[1], "action"),
        Some(&Value::String("rescan".into()))
    );
}

fn refresh_for_event(manager: &WatchManager, event: &Event) {
    let roots = manager.recursive_roots_for_event(event);
    tokio::runtime::Runtime::new()
        .unwrap()
        .block_on(manager.refresh_recursive_roots(roots));
}

#[cfg(target_os = "linux")]
fn refresh_and_rearm(manager: &WatchManager, roots: HashSet<PathBuf>, suspects: HashSet<PathBuf>) {
    tokio::runtime::Runtime::new().unwrap().block_on(async {
        manager.refresh_recursive_roots(roots).await;
        manager.rearm_suspect_paths(suspects).await;
    });
}

#[test]
fn test_rescan_refreshes_all_recursive_roots() {
    let temp = tempfile::tempdir().unwrap();
    let root1 = temp.path().join("root1");
    let root2 = temp.path().join("root2");
    fs::create_dir_all(&root1).unwrap();
    fs::create_dir_all(&root2).unwrap();
    let root1 = root1.canonicalize().unwrap();
    let root2 = root2.canonicalize().unwrap();
    let manager = test_manager();

    manager.watch(&root1, true).unwrap();
    manager.watch(&root2, true).unwrap();
    fs::create_dir(root1.join("new1")).unwrap();
    fs::create_dir(root2.join("new2")).unwrap();

    refresh_for_event(&manager, &Event::new(EventKind::Any).set_flag(Flag::Rescan));

    {
        let watcher = lock_or_recover(&manager.watcher);
        assert!(watcher.recursive_roots[&root1].contains(&root1.join("new1")));
        assert!(watcher.recursive_roots[&root2].contains(&root2.join("new2")));
    }
    manager.unwatch(&root1).unwrap();
    manager.unwatch(&root2).unwrap();
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
    let unignore_event = Event::new(EventKind::Modify(ModifyKind::Any)).add_path(gitignore.clone());
    refresh_for_event(&manager, &unignore_event);
    {
        let watcher = lock_or_recover(&manager.watcher);
        assert!(watcher.recursive_roots[&root].contains(&ignored_dir));
        assert_eq!(watcher.path_watch_counts.get(&ignored_dir), Some(&1));
    }

    fs::write(&gitignore, "ignored/\n").unwrap();
    let ignore_event = Event::new(EventKind::Modify(ModifyKind::Any)).add_path(gitignore.clone());
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

#[cfg(all(target_os = "linux", target_arch = "x86_64", unix))]
#[test]
fn test_recursive_watch_follows_symlinked_directories_for_real_events() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().canonicalize().unwrap();
    let real = root.join("real");
    let link = root.join("link");
    let real_nested = real.join("nested");
    let linked_nested = link.join("nested");
    let linked_file = linked_nested.join("file");
    fs::create_dir_all(&real_nested).unwrap();
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
            .any(|path| path.starts_with(&linked_nested) || path.starts_with(&real_nested))
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
        symlink_watcher: Mutex::new(None),
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
    refresh_and_rearm(&manager, roots, suspects);

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
        symlink_watcher: Mutex::new(None),
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
    refresh_and_rearm(&manager, roots, suspects);

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

#[cfg(unix)]
#[test]
fn test_watch_returns_canonical_path_for_symlink() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().canonicalize().unwrap();
    let real = root.join("real");
    let link = root.join("link");
    fs::create_dir_all(&real).unwrap();
    std::os::unix::fs::symlink(&real, &link).unwrap();

    let manager = test_manager();
    let watched = manager.watch(&link, false).unwrap();

    assert_eq!(watched, real.canonicalize().unwrap());
    assert_eq!(manager.list(), vec![(real.canonicalize().unwrap(), false)]);
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
