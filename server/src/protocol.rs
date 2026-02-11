//! MessagePack-RPC protocol types for TRAMP-RPC

use rmpv::Value;
use serde::{Deserialize, Serialize};

/// Default empty params (nil/null)
fn default_params() -> Value {
    Value::Nil
}

/// RPC request
#[derive(Debug, Deserialize)]
pub struct Request {
    pub version: String,
    pub id: RequestId,
    pub method: String,
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
    pub version: String,
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
            version: "2.0".to_string(),
            id: Some(id),
            result: Some(result.into()),
            error: None,
        }
    }

    pub fn error(id: Option<RequestId>, error: RpcError) -> Self {
        Self {
            version: "2.0".to_string(),
            id,
            result: None,
            error: Some(error),
        }
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
            message: format!("Method not found: {}", method),
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
            message: format!("File not found: {}", path),
            data: None,
        }
    }

    pub fn permission_denied(path: &str) -> Self {
        Self {
            code: Self::PERMISSION_DENIED,
            message: format!("Permission denied: {}", path),
            data: None,
        }
    }

    pub fn io_error(err: std::io::Error) -> Self {
        // Include the raw OS errno in the data field so clients can
        // match on it structurally rather than parsing the message text.
        let data = err.raw_os_error().map(|errno| {
            Value::Map(vec![(
                Value::String("os_errno".into()),
                Value::Integer(rmpv::Integer::from(errno)),
            )])
        });
        Self {
            code: Self::IO_ERROR,
            message: err.to_string(),
            data,
        }
    }
}

/// Server-initiated notification (no id, no response expected)
/// Used for push notifications like filesystem change events.
#[derive(Debug, Serialize)]
pub struct Notification {
    pub version: String,
    pub method: String,
    pub params: Value,
}

impl Notification {
    pub fn new(method: impl Into<String>, params: Value) -> Self {
        Self {
            version: "2.0".to_string(),
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
    /// Last modification time (seconds since epoch)
    pub mtime: i64,
    /// Last status change time (seconds since epoch)
    pub ctime: i64,
    /// File size in bytes
    pub size: u64,
    /// File mode (permissions)
    pub mode: u32,
    /// Inode number
    pub inode: u64,
    /// Device number
    pub dev: u64,
    /// Symlink target (if symlink)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub link_target: Option<String>,
}

impl FileAttributes {
    /// Convert to a MessagePack Value with named fields (map instead of array)
    pub fn to_value(&self) -> Value {
        let file_type_str = match self.file_type {
            FileType::File => "file",
            FileType::Directory => "directory",
            FileType::Symlink => "symlink",
            FileType::CharDevice => "chardevice",
            FileType::BlockDevice => "blockdevice",
            FileType::Fifo => "fifo",
            FileType::Socket => "socket",
            FileType::Unknown => "unknown",
        };

        let mut pairs: Vec<(Value, Value)> = vec![
            (
                Value::String("type".into()),
                Value::String(file_type_str.into()),
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
                Value::String("mtime".into()),
                Value::Integer(self.mtime.into()),
            ),
            (
                Value::String("ctime".into()),
                Value::Integer(self.ctime.into()),
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
        ];

        if let Some(ref uname) = self.uname {
            pairs.push((
                Value::String("uname".into()),
                Value::String(uname.clone().into()),
            ));
        }
        if let Some(ref gname) = self.gname {
            pairs.push((
                Value::String("gname".into()),
                Value::String(gname.clone().into()),
            ));
        }
        if let Some(ref link_target) = self.link_target {
            pairs.push((
                Value::String("link_target".into()),
                Value::String(link_target.clone().into()),
            ));
        }

        Value::Map(pairs)
    }
}

/// Stat result - either attributes or an error
#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum StatResult {
    Ok(FileAttributes),
    Err { error: String },
}

impl StatResult {
    /// Convert to a MessagePack Value with named fields
    pub fn to_value(&self) -> Value {
        match self {
            StatResult::Ok(attrs) => attrs.to_value(),
            StatResult::Err { error } => Value::Map(vec![(
                Value::String("error".into()),
                Value::String(error.clone().into()),
            )]),
        }
    }
}

/// Directory entry - filenames are now raw bytes (binary in MessagePack)
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
    /// Convert to a MessagePack Value with named fields
    pub fn to_value(&self) -> Value {
        let file_type_str = match self.file_type {
            FileType::File => "file",
            FileType::Directory => "directory",
            FileType::Symlink => "symlink",
            FileType::CharDevice => "chardevice",
            FileType::BlockDevice => "blockdevice",
            FileType::Fifo => "fifo",
            FileType::Socket => "socket",
            FileType::Unknown => "unknown",
        };

        let mut pairs: Vec<(Value, Value)> = vec![
            (
                Value::String("name".into()),
                Value::Binary(self.name.clone()),
            ),
            (
                Value::String("type".into()),
                Value::String(file_type_str.into()),
            ),
        ];

        if let Some(ref attrs) = self.attrs {
            pairs.push((Value::String("attrs".into()), attrs.to_value()));
        }

        Value::Map(pairs)
    }
}

// ============================================================================
// Process operation types
// ============================================================================

/// Process execution result - output is now raw bytes
#[derive(Debug, Serialize, Deserialize)]
pub struct ProcessResult {
    pub exit_code: i32,
    /// stdout content as raw bytes
    #[serde(with = "serde_bytes")]
    pub stdout: Vec<u8>,
    /// stderr content as raw bytes
    #[serde(with = "serde_bytes")]
    pub stderr: Vec<u8>,
}

impl ProcessResult {
    /// Convert to a MessagePack Value with named fields
    pub fn to_value(&self) -> Value {
        Value::Map(vec![
            (
                Value::String("exit_code".into()),
                Value::Integer(self.exit_code.into()),
            ),
            (
                Value::String("stdout".into()),
                Value::Binary(self.stdout.clone()),
            ),
            (
                Value::String("stderr".into()),
                Value::Binary(self.stderr.clone()),
            ),
        ])
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

impl IntoValue for bool {
    fn into_value(self) -> Value {
        Value::Boolean(self)
    }
}

impl IntoValue for i32 {
    fn into_value(self) -> Value {
        Value::Integer(self.into())
    }
}

impl IntoValue for i64 {
    fn into_value(self) -> Value {
        Value::Integer(self.into())
    }
}

impl IntoValue for u32 {
    fn into_value(self) -> Value {
        Value::Integer(self.into())
    }
}

impl IntoValue for u64 {
    fn into_value(self) -> Value {
        Value::Integer(self.into())
    }
}

impl IntoValue for usize {
    fn into_value(self) -> Value {
        Value::Integer((self as u64).into())
    }
}

impl IntoValue for String {
    fn into_value(self) -> Value {
        Value::String(self.into())
    }
}

impl IntoValue for &str {
    fn into_value(self) -> Value {
        Value::String(self.to_string().into())
    }
}

impl IntoValue for Vec<u8> {
    fn into_value(self) -> Value {
        Value::Binary(self)
    }
}

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

/// Helper to convert a serializable type to rmpv::Value
pub fn to_value<T: Serialize>(value: &T) -> Result<Value, rmpv::ext::Error> {
    rmpv::ext::to_value(value)
}

/// Helper to deserialize from rmpv::Value to a typed struct
pub fn from_value<T: for<'de> Deserialize<'de>>(value: Value) -> Result<T, rmpv::ext::Error> {
    rmpv::ext::from_value(value)
}

/// Custom deserializer that accepts either a string or binary for paths.
/// Use with `#[serde(with = "crate::protocol::path_or_bytes")]` on Vec<u8> fields.
pub mod path_or_bytes {
    use serde::{self, Deserialize, Deserializer};

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum StringOrBytes {
            String(String),
            #[serde(with = "serde_bytes")]
            Bytes(Vec<u8>),
        }

        match StringOrBytes::deserialize(deserializer)? {
            StringOrBytes::String(s) => Ok(s.into_bytes()),
            StringOrBytes::Bytes(b) => Ok(b),
        }
    }
}
