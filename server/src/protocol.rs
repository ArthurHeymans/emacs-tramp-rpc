//! JSON-RPC 2.0 protocol types for TRAMP-RPC

use serde::{Deserialize, Serialize};

/// JSON-RPC 2.0 request
#[derive(Debug, Deserialize)]
pub struct Request {
    pub jsonrpc: String,
    pub id: RequestId,
    pub method: String,
    #[serde(default)]
    pub params: serde_json::Value,
}

/// Request ID can be a number or string
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum RequestId {
    Number(i64),
    String(String),
}

/// JSON-RPC 2.0 response
#[derive(Debug, Serialize)]
pub struct Response {
    pub jsonrpc: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<RequestId>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<RpcError>,
}

impl Response {
    pub fn success(id: RequestId, result: impl Serialize) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: Some(id),
            result: Some(serde_json::to_value(result).unwrap_or(serde_json::Value::Null)),
            error: None,
        }
    }

    pub fn error(id: Option<RequestId>, error: RpcError) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(error),
        }
    }
}

/// JSON-RPC 2.0 error object
#[derive(Debug, Serialize)]
pub struct RpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,
}

impl RpcError {
    // Standard JSON-RPC error codes
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
        Self {
            code: Self::IO_ERROR,
            message: err.to_string(),
            data: None,
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

/// Stat result - either attributes or an error
#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum StatResult {
    Ok(FileAttributes),
    Err { error: String },
}

/// Directory entry
#[derive(Debug, Serialize, Deserialize)]
pub struct DirEntry {
    pub name: String,
    #[serde(rename = "type")]
    pub file_type: FileType,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attrs: Option<FileAttributes>,
}

// ============================================================================
// Process operation types
// ============================================================================

/// Process execution result
#[derive(Debug, Serialize, Deserialize)]
pub struct ProcessResult {
    pub exit_code: i32,
    /// Base64-encoded stdout
    pub stdout: String,
    /// Base64-encoded stderr
    pub stderr: String,
}

/// Environment variable
#[derive(Debug, Deserialize)]
pub struct EnvVar {
    pub name: String,
    pub value: String,
}
