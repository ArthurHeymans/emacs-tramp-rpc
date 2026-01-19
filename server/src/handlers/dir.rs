//! Directory operations

use crate::protocol::{from_value, to_value, DirEntry, FileType, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::FileTypeExt;
use std::path::Path;
use tokio::fs;

use super::file::{bytes_to_path, get_file_attributes};
use super::HandlerResult;

/// Custom deserializer that accepts either a string or binary for paths
mod path_or_bytes {
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

/// List directory contents
pub async fn list(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Include file attributes for each entry
        #[serde(default)]
        include_attrs: bool,
        /// Include hidden files (starting with .)
        #[serde(default = "default_true")]
        include_hidden: bool,
    }

    fn default_true() -> bool {
        true
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    let mut entries = fs::read_dir(&path)
        .await
        .map_err(|e| super::file::map_io_error(e, &path_str))?;

    let mut results: Vec<DirEntry> = Vec::new();

    // Add . and .. entries
    if params.include_hidden {
        results.push(DirEntry {
            name: b".".to_vec(),
            file_type: FileType::Directory,
            attrs: if params.include_attrs {
                get_file_attributes(&path, false).await.ok()
            } else {
                None
            },
        });

        let parent = path.parent().unwrap_or(&path);
        results.push(DirEntry {
            name: b"..".to_vec(),
            file_type: FileType::Directory,
            attrs: if params.include_attrs {
                get_file_attributes(parent, false).await.ok()
            } else {
                None
            },
        });
    }

    while let Ok(Some(entry)) = entries.next_entry().await {
        // Get filename as raw bytes (no encoding needed!)
        let name_bytes = entry.file_name().as_bytes().to_vec();

        // Skip hidden files if not requested
        let is_hidden = name_bytes.first() == Some(&b'.');
        if !params.include_hidden && is_hidden {
            continue;
        }

        let file_type = match entry.file_type().await {
            Ok(ft) => {
                if ft.is_file() {
                    FileType::File
                } else if ft.is_dir() {
                    FileType::Directory
                } else if ft.is_symlink() {
                    FileType::Symlink
                } else if ft.is_char_device() {
                    FileType::CharDevice
                } else if ft.is_block_device() {
                    FileType::BlockDevice
                } else if ft.is_fifo() {
                    FileType::Fifo
                } else if ft.is_socket() {
                    FileType::Socket
                } else {
                    FileType::Unknown
                }
            }
            Err(_) => FileType::Unknown,
        };

        let attrs = if params.include_attrs {
            get_file_attributes(&entry.path(), true).await.ok()
        } else {
            None
        };

        results.push(DirEntry {
            name: name_bytes,
            file_type,
            attrs,
        });
    }

    // Sort by name
    results.sort_by(|a, b| a.name.cmp(&b.name));

    // Convert to array of map values with named fields
    let values: Vec<Value> = results.iter().map(|e| e.to_value()).collect();
    Ok(Value::Array(values))
}

/// Create a directory
pub async fn create(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Create parent directories if they don't exist
        #[serde(default)]
        parents: bool,
        /// Directory mode (permissions)
        #[serde(default = "default_mode")]
        mode: u32,
    }

    fn default_mode() -> u32 {
        0o755
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    let result = if params.parents {
        fs::create_dir_all(&path).await
    } else {
        fs::create_dir(&path).await
    };

    result.map_err(|e| super::file::map_io_error(e, &path_str))?;

    // Set permissions
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(params.mode);
        fs::set_permissions(&path, perms)
            .await
            .map_err(|e| super::file::map_io_error(e, &path_str))?;
    }

    Ok(Value::Boolean(true))
}

/// Remove a directory
pub async fn remove(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Remove recursively
        #[serde(default)]
        recursive: bool,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    let result = if params.recursive {
        fs::remove_dir_all(&path).await
    } else {
        fs::remove_dir(&path).await
    };

    result.map_err(|e| super::file::map_io_error(e, &path_str))?;

    Ok(Value::Boolean(true))
}

/// Get completions for a path prefix
pub async fn completions(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Directory to search in
        directory: String,
        /// Prefix to complete
        prefix: String,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let dir_path = Path::new(&params.directory);

    let mut entries = fs::read_dir(dir_path)
        .await
        .map_err(|e| super::file::map_io_error(e, &params.directory))?;

    let mut completions: Vec<String> = Vec::new();

    while let Ok(Some(entry)) = entries.next_entry().await {
        let name = entry.file_name().to_string_lossy().to_string();

        // Check if name starts with prefix
        if name.starts_with(&params.prefix) {
            // Append / for directories
            let suffix = if entry
                .file_type()
                .await
                .map(|ft| ft.is_dir())
                .unwrap_or(false)
            {
                "/"
            } else {
                ""
            };
            completions.push(format!("{}{}", name, suffix));
        }
    }

    completions.sort();

    to_value(&completions).map_err(|e| RpcError::internal_error(e.to_string()))
}
