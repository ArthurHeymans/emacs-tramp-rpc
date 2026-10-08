// SPDX-License-Identifier: GPL-3.0-or-later

//! Kernel access checks using effective credentials, never mode-bit emulation.

use rustix::fs::{Access, AtFlags};
use rustix::io;
use std::path::Path;

/// Distinguish filesystem denials from checks the server cannot resolve.
#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Filesystem(io::Errno),
    Unsupported(io::Errno),
}

impl From<io::Errno> for Error {
    fn from(errno: io::Errno) -> Self {
        if errno == io::Errno::NOSYS {
            Self::Unsupported(errno)
        } else {
            Self::Filesystem(errno)
        }
    }
}

/// Check access like native Emacs, following symlinks.
pub fn check(path: &Path, mode: Access) -> Result<(), Error> {
    #[cfg(target_os = "linux")]
    {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt;

        let c_path = CString::new(path.as_os_str().as_bytes()).map_err(|_| io::Errno::INVAL)?;
        // rustix silently retries ENOSYS with real-ID faccessat when IDs match,
        // without checking capabilities. Keep that fallback under our control.
        // SAFETY: c_path is NUL-terminated and lives through the syscall; the
        // remaining arguments are the kernel's scalar faccessat2 parameters.
        let result = unsafe {
            libc::syscall(
                libc::SYS_faccessat2,
                libc::AT_FDCWD,
                c_path.as_ptr(),
                mode.bits(),
                libc::AT_EACCESS,
            )
        };
        if result == 0 {
            return Ok(());
        }
        let error = io::Errno::from_io_error(&std::io::Error::last_os_error())
            .expect("failed syscall sets errno");
        // Older kernels return ENOSYS; older container filters can use EPERM.
        // Never substitute a real-ID check if its answer could differ.
        if matches!(error, io::Errno::NOSYS | io::Errno::PERM) {
            if real_credentials_equivalent() {
                return rustix::fs::accessat(rustix::fs::CWD, path, mode, AtFlags::empty())
                    .map_err(Error::from);
            }
            // EPERM may come from a syscall filter, not the filesystem.  If
            // there is no safe retry, neither errno proves an access denial.
            return Err(Error::Unsupported(error));
        }
        Err(error.into())
    }
    #[cfg(not(target_os = "linux"))]
    {
        rustix::fs::accessat(rustix::fs::CWD, path, mode, AtFlags::EACCESS).map_err(Error::from)
    }
}

#[cfg(target_os = "linux")]
fn real_credentials_equivalent() -> bool {
    let uid = rustix::process::getuid();
    let gid = rustix::process::getgid();
    uid == rustix::process::geteuid()
        && gid == rustix::process::getegid()
        // The reserved -1 IDs query fsuid/fsgid without changing them.
        && nix::unistd::setfsuid(nix::unistd::Uid::from_raw(!0)).as_raw() == uid.as_raw()
        && nix::unistd::setfsgid(nix::unistd::Gid::from_raw(!0)).as_raw() == gid.as_raw()
        && rustix::thread::capabilities(None)
            .is_ok_and(|caps| capabilities_equivalent(uid.is_root(), caps))
        && id_is_unambiguous(uid.as_raw(), "uid")
        && id_is_unambiguous(gid.as_raw(), "gid")
}

#[cfg(target_os = "linux")]
fn id_is_unambiguous(id: u32, kind: &str) -> bool {
    // Unmapped kernel IDs all appear as the overflow ID. Equal reported IDs
    // therefore do not prove equal credentials in a partial user namespace.
    let Ok(overflow) = std::fs::read_to_string(format!("/proc/sys/kernel/overflow{kind}")) else {
        return false;
    };
    let Ok(overflow) = overflow.trim().parse::<u32>() else {
        return false;
    };
    id != overflow
        || std::fs::read_to_string(format!("/proc/thread-self/{kind}_map"))
            .is_ok_and(|map| namespace_is_fully_mapped(&map))
}

#[cfg(target_os = "linux")]
fn namespace_is_fully_mapped(map: &str) -> bool {
    map.split_whitespace().eq(["0", "0", &u32::MAX.to_string()])
}

#[cfg(target_os = "linux")]
fn capabilities_equivalent(root: bool, caps: rustix::thread::CapabilitySets) -> bool {
    // Real-ID access uses permitted capabilities for root, none for others.
    if root {
        caps.effective == caps.permitted
    } else {
        caps.effective.is_empty()
    }
}

#[cfg(all(test, target_os = "linux"))]
mod tests {
    use super::*;
    use rustix::thread::{CapabilitySet, CapabilitySets};

    #[test]
    fn overflow_ids_require_a_complete_namespace_mapping() {
        assert!(namespace_is_fully_mapped(
            "         0          0 4294967295\n"
        ));
        for map in ["", "0 1000 1\n", "1000 1000 1\n", "0 0 65536\n"] {
            assert!(!namespace_is_fully_mapped(map));
        }
    }

    #[test]
    fn credential_equivalence_includes_capabilities() {
        for (root, effective, permitted, expected) in [
            (false, CapabilitySet::empty(), CapabilitySet::empty(), true),
            (
                false,
                CapabilitySet::empty(),
                CapabilitySet::DAC_OVERRIDE,
                true,
            ),
            (
                false,
                CapabilitySet::DAC_OVERRIDE,
                CapabilitySet::DAC_OVERRIDE,
                false,
            ),
            (
                true,
                CapabilitySet::DAC_OVERRIDE,
                CapabilitySet::DAC_OVERRIDE,
                true,
            ),
            (
                true,
                CapabilitySet::empty(),
                CapabilitySet::DAC_OVERRIDE,
                false,
            ),
        ] {
            assert_eq!(
                capabilities_equivalent(
                    root,
                    CapabilitySets {
                        effective,
                        permitted,
                        inheritable: CapabilitySet::empty()
                    }
                ),
                expected
            );
        }
    }

    #[test]
    fn blocked_faccessat2_uses_only_equivalent_credentials() {
        const ENV: &str = "TRAMP_RPC_ACCESS_TEST_SECCOMP";
        if let Ok(setting) = std::env::var(ENV) {
            let (errno, deny_capget) = setting.split_once(':').unwrap();
            let errno: i32 = errno.parse().unwrap();
            let deny_capget: bool = deny_capget.parse().unwrap();
            let temp = tempfile::tempdir().unwrap();
            let file = temp.path().join("readable");
            std::fs::write(&file, "contents").unwrap();
            let equivalent = !deny_capget && real_credentials_equivalent();
            block_access_syscalls(errno, deny_capget);
            let result = check(&file, Access::READ_OK);
            if equivalent {
                assert_eq!(result, Ok(()));
                assert_eq!(
                    check(&file.with_file_name("missing"), Access::READ_OK),
                    Err(Error::Filesystem(io::Errno::NOENT))
                );
            } else {
                assert_eq!(
                    result,
                    Err(Error::Unsupported(io::Errno::from_raw_os_error(errno)))
                );
                assert_eq!(std::fs::read(&file).unwrap(), b"contents");
                assert_unsupported_dispatch(&file, errno);
            }
            return;
        }
        for (errno, deny_capget) in [
            (libc::ENOSYS, false),
            (libc::EPERM, false),
            (libc::ENOSYS, true),
            (libc::EPERM, true),
        ] {
            let status = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "access::tests::blocked_faccessat2_uses_only_equivalent_credentials",
                    "--nocapture",
                ])
                .env(ENV, format!("{errno}:{deny_capget}"))
                .status()
                .unwrap();
            assert!(status.success());
        }
    }

    fn assert_unsupported_dispatch(path: &Path, errno: i32) {
        use crate::protocol::{Request, RequestId, RpcError, from_value};
        use rmpv::Value;
        use std::collections::HashMap;
        use std::os::unix::ffi::OsStrExt;

        let params = crate::msgpack_map! {
            "path" => Value::Binary(path.as_os_str().as_bytes().to_vec()),
            "mode" => "r",
        };
        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async {
                let single = crate::handlers::dispatch(Request {
                    version: "2.0".into(),
                    id: RequestId::Number(1),
                    method: "file.access".into(),
                    params: params.clone(),
                })
                .await;
                assert!(single.result.is_none());
                let error = single.error.unwrap();
                assert_eq!(error.code, RpcError::IO_ERROR);
                let data: HashMap<String, i32> = from_value(error.data.unwrap()).unwrap();
                assert_eq!(data["os_errno"], errno);

                let batch = crate::handlers::dispatch(Request {
                    version: "2.0".into(),
                    id: RequestId::Number(2),
                    method: "batch".into(),
                    params: crate::msgpack_map! {
                        "requests" => Value::Array(vec![crate::msgpack_map! {
                            "method" => "file.access", "params" => params,
                        }]),
                    },
                })
                .await;
                assert!(batch.error.is_none());
                let result: HashMap<String, Vec<HashMap<String, Value>>> =
                    from_value(batch.result.unwrap()).unwrap();
                let entry = &result["results"][0];
                assert!(!entry.contains_key("result"));
                let error: HashMap<String, Value> = from_value(entry["error"].clone()).unwrap();
                assert_eq!(error["code"].as_i64(), Some(i64::from(RpcError::IO_ERROR)));
                let data: HashMap<String, i32> = from_value(error["data"].clone()).unwrap();
                assert_eq!(data["os_errno"], errno);
            });
    }

    fn block_access_syscalls(errno: i32, deny_capget: bool) {
        // The filter affects only the re-executed test process, not the runner.
        let filter = [
            libc::sock_filter {
                code: (libc::BPF_LD | libc::BPF_W | libc::BPF_ABS) as u16,
                jt: 0,
                jf: 0,
                k: 0,
            },
            libc::sock_filter {
                code: (libc::BPF_JMP | libc::BPF_JEQ | libc::BPF_K) as u16,
                jt: 0,
                jf: 1,
                k: libc::SYS_faccessat2 as u32,
            },
            libc::sock_filter {
                code: (libc::BPF_RET | libc::BPF_K) as u16,
                jt: 0,
                jf: 0,
                k: libc::SECCOMP_RET_ERRNO | errno as u32,
            },
            libc::sock_filter {
                code: (libc::BPF_JMP | libc::BPF_JEQ | libc::BPF_K) as u16,
                jt: 0,
                jf: 1,
                k: if deny_capget {
                    libc::SYS_capget
                } else {
                    libc::SYS_faccessat2
                } as u32,
            },
            libc::sock_filter {
                code: (libc::BPF_RET | libc::BPF_K) as u16,
                jt: 0,
                jf: 0,
                k: libc::SECCOMP_RET_ERRNO | libc::EPERM as u32,
            },
            libc::sock_filter {
                code: (libc::BPF_RET | libc::BPF_K) as u16,
                jt: 0,
                jf: 0,
                k: libc::SECCOMP_RET_ALLOW,
            },
        ];
        let program = libc::sock_fprog {
            len: filter.len() as u16,
            filter: filter.as_ptr().cast_mut(),
        };
        // SAFETY: prctl reads the valid program/filter during the call. The
        // filter only denies one syscall and is installed in an isolated child.
        unsafe {
            assert_eq!(libc::prctl(libc::PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0), 0);
            assert_eq!(
                libc::prctl(libc::PR_SET_SECCOMP, libc::SECCOMP_MODE_FILTER, &program),
                0
            );
        }
    }
}
