// SPDX-License-Identifier: GPL-3.0-or-later

//! MessagePack-RPC protocol types for TRAMP-RPC

use rmpv::Value;
use serde::{Deserialize, Deserializer, Serialize, de};
use std::fmt;
use std::path::PathBuf;

/// Client/server contract revision, independent of release and framing versions.
/// Bump together with `tramp-rpc-protocol-revision` for incompatible RPC changes.
pub const PROTOCOL_REVISION: u64 = 4;

/// Default empty params (nil/null)
fn default_params() -> Value {
    Value::Nil
}

/// Typed RPC method parsed once during request decoding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Method {
    Batch,
    FileStat,
    FileAccess,
    FileTruename,
    DirList,
    DirCreate,
    DirRemove,
    FileRead,
    FileWrite,
    FileCopy,
    FileRename,
    FileDelete,
    FileSetModes,
    FileSetTimes,
    FileMakeSymlink,
    FileMakeHardlink,
    FileChown,
    ProcessRun,
    ProcessStart,
    ProcessWrite,
    ProcessCloseStdin,
    ProcessKill,
    ProcessSignal,
    ProcessStartPty,
    ProcessWritePty,
    ProcessResizePty,
    ProcessKillPty,
    ProcessClosePty,
    SystemPing,
    SystemInfo,
    SystemStatvfs,
    SystemUserHome,
    CommandsRunParallel,
    AncestorsScan,
    HighlevelTestFilesInDir,
    HighlevelLocateDominatingFileMulti,
    HighlevelDirLocalsFindFileCacheUpdate,
    WatchAdd,
    WatchRemove,
    Unknown(String),
}

impl Method {
    pub fn parse(s: &str) -> Self {
        match s {
            "batch" => Self::Batch,
            "file.stat" => Self::FileStat,
            "file.access" => Self::FileAccess,
            "file.truename" => Self::FileTruename,
            "dir.list" => Self::DirList,
            "dir.create" => Self::DirCreate,
            "dir.remove" => Self::DirRemove,
            "file.read" => Self::FileRead,
            "file.write" => Self::FileWrite,
            "file.copy" => Self::FileCopy,
            "file.rename" => Self::FileRename,
            "file.delete" => Self::FileDelete,
            "file.set_modes" => Self::FileSetModes,
            "file.set_times" => Self::FileSetTimes,
            "file.make_symlink" => Self::FileMakeSymlink,
            "file.make_hardlink" => Self::FileMakeHardlink,
            "file.chown" => Self::FileChown,
            "process.run" => Self::ProcessRun,
            "process.start" => Self::ProcessStart,
            "process.write" => Self::ProcessWrite,
            "process.close_stdin" => Self::ProcessCloseStdin,
            "process.kill" => Self::ProcessKill,
            "process.signal" => Self::ProcessSignal,
            "process.start_pty" => Self::ProcessStartPty,
            "process.write_pty" => Self::ProcessWritePty,
            "process.resize_pty" => Self::ProcessResizePty,
            "process.kill_pty" => Self::ProcessKillPty,
            "process.close_pty" => Self::ProcessClosePty,
            "system.ping" => Self::SystemPing,
            "system.info" => Self::SystemInfo,
            "system.statvfs" => Self::SystemStatvfs,
            "system.user_home" => Self::SystemUserHome,
            "commands.run_parallel" => Self::CommandsRunParallel,
            "ancestors.scan" => Self::AncestorsScan,
            "highlevel.test_files_in_dir" => Self::HighlevelTestFilesInDir,
            "highlevel.locate_dominating_file_multi" => Self::HighlevelLocateDominatingFileMulti,
            "highlevel.dir_locals_find_file_cache_update" => {
                Self::HighlevelDirLocalsFindFileCacheUpdate
            }
            "watch.add" => Self::WatchAdd,
            "watch.remove" => Self::WatchRemove,
            other => Self::Unknown(other.to_owned()),
        }
    }

    pub fn is_control(&self) -> bool {
        matches!(
            self,
            Self::SystemPing
                | Self::ProcessKill
                | Self::ProcessSignal
                | Self::ProcessCloseStdin
                | Self::ProcessKillPty
                | Self::ProcessClosePty
        )
    }

    pub fn is_process_write(&self) -> bool {
        matches!(self, Self::ProcessWrite | Self::ProcessWritePty)
    }

    pub fn permit_count(&self) -> usize {
        if *self == Self::Batch {
            crate::handlers::BATCH_CONCURRENCY
        } else {
            1
        }
    }
}

impl<'de> Deserialize<'de> for Method {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct MethodVisitor;

        impl<'de> de::Visitor<'de> for MethodVisitor {
            type Value = Method;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("an RPC method name string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Method::parse(v))
            }
        }

        deserializer.deserialize_str(MethodVisitor)
    }
}

/// Raw Unix path bytes deserialized from either a MessagePack string or binary value
/// without intermediate untagged-enum buffering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RpcPath(pub Vec<u8>);

impl RpcPath {
    pub async fn resolve(&self) -> Result<PathBuf, RpcError> {
        crate::handlers::file::bytes_to_path(&self.0).await
    }

    pub fn resolve_sync(&self) -> PathBuf {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;
        match crate::handlers::system::expand_tilde_bytes(&self.0) {
            Some(expanded) => PathBuf::from(OsStr::from_bytes(&expanded)),
            None => PathBuf::from(OsStr::from_bytes(&self.0)),
        }
    }
}

impl<'de> Deserialize<'de> for RpcPath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct RpcPathVisitor;

        impl<'de> de::Visitor<'de> for RpcPathVisitor {
            type Value = RpcPath;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a path string or binary byte buffer")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(RpcPath(v.as_bytes().to_vec()))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(RpcPath(v.into_bytes()))
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(RpcPath(v.to_vec()))
            }

            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(RpcPath(v))
            }
        }

        deserializer.deserialize_any(RpcPathVisitor)
    }
}

/// RPC request
#[derive(Debug, Deserialize)]
pub struct Request {
    pub version: String,
    pub id: RequestId,
    pub method: Method,
    #[serde(default = "default_params")]
    pub params: Value,
}

/// Request ID can be a number or string
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum RequestId {
    Number(i64),
    String(String),
}

/// RPC response
#[derive(Debug, Serialize)]
pub struct Response {
    pub version: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<RequestId>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<RpcError>,
}

impl Response {
    pub fn success(id: RequestId, result: impl Into<Value>) -> Self {
        Self {
            version: "2.0",
            id: Some(id),
            result: Some(result.into()),
            error: None,
        }
    }

    pub fn error(id: Option<RequestId>, error: RpcError) -> Self {
        Self {
            version: "2.0",
            id,
            result: None,
            error: Some(error),
        }
    }
}

/// Post-send action attached to a dispatched response so side effects (such as
/// starting process output push) run strictly after the response frame is sent,
/// without re-parsing the serialized response map.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostSendAction {
    StartOutputPush(u32),
}

/// Result of dispatching one RPC request.
#[derive(Debug)]
pub struct DispatchOutcome {
    pub response: Response,
    pub post_send: Option<PostSendAction>,
}

impl DispatchOutcome {
    pub fn new(response: Response) -> Self {
        Self {
            response,
            post_send: None,
        }
    }

    pub fn with_post_send(response: Response, post_send: PostSendAction) -> Self {
        Self {
            response,
            post_send: Some(post_send),
        }
    }
}

/// Classify a `std::io::Error` into a portable error kind name so clients never
/// have to hardcode OS-specific errno numbers (e.g. Linux vs macOS `ELOOP`/`ENOTEMPTY`).
pub fn portable_io_error_kind(err: &std::io::Error) -> Option<&'static str> {
    if let Some(errno) = err.raw_os_error() {
        let by_errno = match errno {
            libc::ENOENT => Some("not_found"),
            libc::EACCES | libc::EPERM => Some("permission_denied"),
            libc::EEXIST => Some("already_exists"),
            libc::ENOTDIR => Some("not_a_directory"),
            libc::EISDIR => Some("is_a_directory"),
            libc::ENOTEMPTY => Some("directory_not_empty"),
            libc::ELOOP => Some("symlink_loop"),
            libc::EXDEV => Some("cross_device"),
            libc::ENAMETOOLONG => Some("name_too_long"),
            libc::EOPNOTSUPP | libc::ENOSYS => Some("unsupported"),
            libc::EINVAL => Some("invalid_input"),
            libc::EROFS => Some("read_only_filesystem"),
            libc::ENOSPC => Some("no_space"),
            libc::EBUSY => Some("resource_busy"),
            _ => None,
        };
        if by_errno.is_some() {
            return by_errno;
        }
    }

    use std::io::ErrorKind;
    match err.kind() {
        ErrorKind::NotFound => Some("not_found"),
        ErrorKind::PermissionDenied => Some("permission_denied"),
        ErrorKind::AlreadyExists => Some("already_exists"),
        ErrorKind::NotADirectory => Some("not_a_directory"),
        ErrorKind::IsADirectory => Some("is_a_directory"),
        ErrorKind::DirectoryNotEmpty => Some("directory_not_empty"),
        ErrorKind::InvalidInput => Some("invalid_input"),
        ErrorKind::Unsupported => Some("unsupported"),
        ErrorKind::ReadOnlyFilesystem => Some("read_only_filesystem"),
        ErrorKind::StorageFull => Some("no_space"),
        ErrorKind::ResourceBusy => Some("resource_busy"),
        _ => None,
    }
}

/// RPC error object
#[derive(Debug, Serialize)]
pub struct RpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

impl RpcError {
    // Standard RPC error codes
    pub const PARSE_ERROR: i32 = -32700;
    pub const INVALID_REQUEST: i32 = -32600;
    pub const METHOD_NOT_FOUND: i32 = -32601;
    pub const INVALID_PARAMS: i32 = -32602;
    pub const INTERNAL_ERROR: i32 = -32603;

    // Custom error codes for file operations
    pub const FILE_NOT_FOUND: i32 = -32001;
    pub const PERMISSION_DENIED: i32 = -32002;
    pub const IO_ERROR: i32 = -32003;
    pub const PROCESS_ERROR: i32 = -32004;
    /// Admission rejected the request before it started; it had no effect
    /// and may be sent again.
    pub const SERVER_BUSY: i32 = -32005;

    pub fn server_busy(msg: impl Into<String>) -> Self {
        Self {
            code: Self::SERVER_BUSY,
            message: msg.into(),
            data: None,
        }
    }

    pub fn parse_error(msg: impl Into<String>) -> Self {
        Self {
            code: Self::PARSE_ERROR,
            message: msg.into(),
            data: None,
        }
    }

    pub fn invalid_request(msg: impl Into<String>) -> Self {
        Self {
            code: Self::INVALID_REQUEST,
            message: msg.into(),
            data: None,
        }
    }

    pub fn method_not_found(method: &str) -> Self {
        Self {
            code: Self::METHOD_NOT_FOUND,
            message: format!("Method not found: {method}"),
            data: None,
        }
    }

    pub fn invalid_params(msg: impl Into<String>) -> Self {
        Self {
            code: Self::INVALID_PARAMS,
            message: msg.into(),
            data: None,
        }
    }

    pub fn internal_error(msg: impl Into<String>) -> Self {
        Self {
            code: Self::INTERNAL_ERROR,
            message: msg.into(),
            data: None,
        }
    }

    pub fn file_not_found(path: &str) -> Self {
        Self {
            code: Self::FILE_NOT_FOUND,
            message: format!("File not found: {path}"),
            data: None,
        }
    }

    pub fn permission_denied(path: &str) -> Self {
        Self {
            code: Self::PERMISSION_DENIED,
            message: format!("Permission denied: {path}"),
            data: None,
        }
    }

    pub fn process_error(msg: impl Into<String>) -> Self {
        Self {
            code: Self::PROCESS_ERROR,
            message: msg.into(),
            data: None,
        }
    }

    pub fn process_error_with_kind(msg: impl Into<String>, kind: &str) -> Self {
        Self {
            code: Self::PROCESS_ERROR,
            message: msg.into(),
            data: Some(Value::Map(vec![(
                Value::String("process_error".into()),
                Value::String(kind.into()),
            )])),
        }
    }

    pub fn io_error(err: std::io::Error) -> Self {
        // Include both a portable `error_kind` string and the raw `os_errno`
        // so clients can classify errors cross-platform without hardcoding
        // Linux-only errno numbers.
        let mut fields = Vec::new();
        if let Some(kind) = portable_io_error_kind(&err) {
            fields.push((
                Value::String("error_kind".into()),
                Value::String(kind.into()),
            ));
        }
        if let Some(errno) = err.raw_os_error() {
            fields.push((
                Value::String("os_errno".into()),
                Value::Integer(rmpv::Integer::from(errno)),
            ));
        }
        let data = (!fields.is_empty()).then_some(Value::Map(fields));
        Self {
            code: Self::IO_ERROR,
            message: err.to_string(),
            data,
        }
    }

    /// Replace the portable `error_kind` derived from the errno, for errors
    /// whose errno does not describe what happened.
    pub fn with_error_kind(mut self, kind: &str) -> Self {
        let kind_field = (
            Value::String("error_kind".into()),
            Value::String(kind.into()),
        );
        match &mut self.data {
            Some(Value::Map(fields)) => {
                fields.retain(|(key, _)| key.as_str() != Some("error_kind"));
                fields.insert(0, kind_field);
            }
            _ => self.data = Some(Value::Map(vec![kind_field])),
        }
        self
    }
}

/// Server-initiated notification (no id, no response expected)
/// Used for push notifications like filesystem change events.
#[derive(Debug, Serialize)]
pub struct Notification {
    pub version: &'static str,
    pub method: String,
    pub params: Value,
}

impl Notification {
    pub fn new(method: impl Into<String>, params: Value) -> Self {
        Self {
            version: "2.0",
            method: method.into(),
            params,
        }
    }
}

// ============================================================================
// File operation types
// ============================================================================

/// File type enumeration
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum FileType {
    File,
    Directory,
    Symlink,
    CharDevice,
    BlockDevice,
    Fifo,
    Socket,
    Unknown,
}

impl FileType {
    /// Return the string representation of this file type.
    pub fn as_str(&self) -> &'static str {
        match self {
            FileType::File => "file",
            FileType::Directory => "directory",
            FileType::Symlink => "symlink",
            FileType::CharDevice => "chardevice",
            FileType::BlockDevice => "blockdevice",
            FileType::Fifo => "fifo",
            FileType::Socket => "socket",
            FileType::Unknown => "unknown",
        }
    }
}

/// File attributes (similar to Emacs file-attributes)
#[derive(Debug, Serialize, Deserialize)]
pub struct FileAttributes {
    /// File type
    #[serde(rename = "type")]
    pub file_type: FileType,
    /// Number of hard links
    pub nlinks: u64,
    /// User ID
    pub uid: u32,
    /// Group ID
    pub gid: u32,
    /// User name (resolved from uid)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uname: Option<String>,
    /// Group name (resolved from gid)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gname: Option<String>,
    /// Last access time (seconds since epoch)
    pub atime: i64,
    /// Last access time nanoseconds component (0..1_000_000_000)
    #[serde(default)]
    pub atime_nsec: i64,
    /// Last modification time (seconds since epoch)
    pub mtime: i64,
    /// Last modification time nanoseconds component (0..1_000_000_000)
    #[serde(default)]
    pub mtime_nsec: i64,
    /// Last status change time (seconds since epoch)
    pub ctime: i64,
    /// Last status change time nanoseconds component (0..1_000_000_000)
    #[serde(default)]
    pub ctime_nsec: i64,
    /// File size in bytes
    pub size: u64,
    /// File mode (permissions)
    pub mode: u32,
    /// Inode number
    pub inode: u64,
    /// Device number
    pub dev: u64,
    /// Symlink target as raw bytes (if symlink)
    #[serde(default, with = "serde_bytes", skip_serializing_if = "Option::is_none")]
    pub link_target: Option<Vec<u8>>,
}

impl FileAttributes {
    /// Convert to a MessagePack Value with named fields, consuming self without cloning.
    pub fn into_value(self) -> Value {
        let mut pairs: Vec<(Value, Value)> = Vec::with_capacity(17);
        pairs.extend([
            (
                Value::String("type".into()),
                Value::String(self.file_type.as_str().into()),
            ),
            (
                Value::String("nlinks".into()),
                Value::Integer(self.nlinks.into()),
            ),
            (Value::String("uid".into()), Value::Integer(self.uid.into())),
            (Value::String("gid".into()), Value::Integer(self.gid.into())),
            (
                Value::String("atime".into()),
                Value::Integer(self.atime.into()),
            ),
            (
                Value::String("atime_nsec".into()),
                Value::Integer(self.atime_nsec.into()),
            ),
            (
                Value::String("mtime".into()),
                Value::Integer(self.mtime.into()),
            ),
            (
                Value::String("mtime_nsec".into()),
                Value::Integer(self.mtime_nsec.into()),
            ),
            (
                Value::String("ctime".into()),
                Value::Integer(self.ctime.into()),
            ),
            (
                Value::String("ctime_nsec".into()),
                Value::Integer(self.ctime_nsec.into()),
            ),
            (
                Value::String("size".into()),
                Value::Integer(self.size.into()),
            ),
            (
                Value::String("mode".into()),
                Value::Integer(self.mode.into()),
            ),
            (
                Value::String("inode".into()),
                Value::Integer(self.inode.into()),
            ),
            (Value::String("dev".into()), Value::Integer(self.dev.into())),
        ]);

        if let Some(uname) = self.uname {
            pairs.push((Value::String("uname".into()), Value::String(uname.into())));
        }
        if let Some(gname) = self.gname {
            pairs.push((Value::String("gname".into()), Value::String(gname.into())));
        }
        if let Some(link_target) = self.link_target {
            pairs.push((
                Value::String("link_target".into()),
                Value::Binary(link_target),
            ));
        }

        Value::Map(pairs)
    }
}

/// Directory entry - filenames are raw bytes (binary in MessagePack)
#[derive(Debug, Serialize, Deserialize)]
pub struct DirEntry {
    /// Filename as raw bytes (MessagePack bin type handles non-UTF8)
    #[serde(with = "serde_bytes")]
    pub name: Vec<u8>,
    #[serde(rename = "type")]
    pub file_type: FileType,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attrs: Option<FileAttributes>,
}

impl DirEntry {
    /// Convert to a MessagePack Value with named fields, consuming self without cloning.
    pub fn into_value(self) -> Value {
        let mut pairs: Vec<(Value, Value)> = Vec::with_capacity(3);
        pairs.push((Value::String("name".into()), Value::Binary(self.name)));
        pairs.push((
            Value::String("type".into()),
            Value::String(self.file_type.as_str().into()),
        ));

        if let Some(attrs) = self.attrs {
            pairs.push((Value::String("attrs".into()), attrs.into_value()));
        }

        Value::Map(pairs)
    }
}

// ============================================================================
// Process operation types
// ============================================================================

/// Result of a completed `process.run` child.
#[derive(Debug)]
pub struct ProcessResult {
    pub status: std::process::ExitStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl ProcessResult {
    /// Convert to a MessagePack map with `exit_code`, `signal`, `stdout` and
    /// `stderr` fields.
    pub fn into_value(self) -> Value {
        let mut pairs = Vec::from(exit_fields(Some(self.status)));
        pairs.push((Value::String("stdout".into()), Value::Binary(self.stdout)));
        pairs.push((Value::String("stderr".into()), Value::Binary(self.stderr)));
        Value::Map(pairs)
    }
}

// ============================================================================
// Helper macros and functions for constructing MessagePack values
// ============================================================================

/// Create a MessagePack map value from key-value pairs
#[macro_export]
macro_rules! msgpack_map {
    ($($key:expr => $value:expr),* $(,)?) => {{
        let pairs: Vec<(rmpv::Value, rmpv::Value)> = vec![
            $(
                (rmpv::Value::String($key.into()), $value.into()),
            )*
        ];
        rmpv::Value::Map(pairs)
    }};
}

/// Convert various types to rmpv::Value
pub trait IntoValue {
    fn into_value(self) -> Value;
}

/// Implement `IntoValue` for every type with an existing `From<T> for Value`.
macro_rules! impl_into_value_from {
    ($($t:ty),* $(,)?) => {
        $(
            impl IntoValue for $t {
                fn into_value(self) -> Value {
                    self.into()
                }
            }
        )*
    };
}

impl_into_value_from!(bool, i32, i64, u32, u64, usize, String, &str, Vec<u8>);

impl IntoValue for Vec<String> {
    fn into_value(self) -> Value {
        Value::Array(self.into_iter().map(|s| Value::String(s.into())).collect())
    }
}

impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(self) -> Value {
        match self {
            Some(v) => v.into_value(),
            None => Value::Nil,
        }
    }
}

impl IntoValue for Value {
    fn into_value(self) -> Value {
        self
    }
}

/// Return the signal number that terminated STATUS, or `None` for a normal exit.
///
/// Kept separate from [`exit_code_from_status`] because the 128 + signal
/// convention cannot tell a process killed by SIGKILL from one that exited
/// with code 137.
pub fn exit_signal_from_status(status: std::process::ExitStatus) -> Option<i32> {
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;

        // Some platforms report `signal() == None` for a signal death, so
        // fall back to parsing the raw wait status (WIFSIGNALED).
        status.signal().or_else(|| {
            let termsig = status.into_raw() & 0x7f;
            (termsig != 0 && termsig != 0x7f).then_some(termsig)
        })
    }
    #[cfg(not(unix))]
    {
        let _ = status;
        None
    }
}

/// Extract an exit code from a `std::process::ExitStatus`.
///
/// Signal deaths use the shell convention 128 + signal (SIGKILL -> 137).
pub fn exit_code_from_status(status: std::process::ExitStatus) -> i32 {
    status
        .code()
        .or_else(|| exit_signal_from_status(status).map(|signal| 128 + signal))
        .unwrap_or(-1)
}

/// Terminal fields shared by `process.run` results and `process.exit`
/// notifications: the historical `exit_code` plus the terminating `signal`.
pub fn exit_fields(status: Option<std::process::ExitStatus>) -> [(Value, Value); 2] {
    let int_or_nil = |value: Option<i32>| value.map_or(Value::Nil, |v| Value::Integer(v.into()));
    [
        (
            Value::String("exit_code".into()),
            int_or_nil(status.map(exit_code_from_status)),
        ),
        (
            Value::String("signal".into()),
            int_or_nil(status.and_then(exit_signal_from_status)),
        ),
    ]
}

/// Helper to deserialize from rmpv::Value to a typed struct
pub fn from_value<T: for<'de> Deserialize<'de>>(value: Value) -> Result<T, rmpv::ext::Error> {
    rmpv::ext::from_value(value)
}

/// Zero-allocation `io::Write` sink that only counts serialized bytes.
/// Used for incremental size accounting without allocating throwaway buffers.
#[derive(Default)]
pub struct CountingWriter {
    pub count: usize,
}

impl std::io::Write for CountingWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.count = self
            .count
            .checked_add(buf.len())
            .ok_or_else(|| std::io::Error::other("encoded size overflow"))?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// Compute the encoded MessagePack byte length of `value` without allocating.
pub fn encoded_msgpack_len<T: Serialize>(value: &T) -> Result<usize, rmp_serde::encode::Error> {
    let mut counter = CountingWriter::default();
    rmp_serde::encode::write_named(&mut counter, value)?;
    Ok(counter.count)
}
