//! Directory operations

use crate::protocol::{DirEntry, FileType, RpcError};
use serde::Deserialize;
use std::os::unix::fs::FileTypeExt;
use std::path::Path;
use tokio::fs;

use super::file::get_file_attributes;

type HandlerResult = Result<serde_json::Value, RpcError>;

/// List directory contents
pub async fn list(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
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

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = Path::new(&params.path);

    let mut entries = fs::read_dir(path)
        .await
        .map_err(|e| super::file::map_io_error(e, &params.path))?;

    let mut results: Vec<DirEntry> = Vec::new();

    // Add . and .. entries
    if params.include_hidden {
        results.push(DirEntry {
            name: ".".to_string(),
            file_type: FileType::Directory,
            attrs: if params.include_attrs {
                get_file_attributes(&params.path, false).await.ok()
            } else {
                None
            },
        });

        let parent = path.parent().unwrap_or(path);
        results.push(DirEntry {
            name: "..".to_string(),
            file_type: FileType::Directory,
            attrs: if params.include_attrs {
                get_file_attributes(&parent.to_string_lossy(), false)
                    .await
                    .ok()
            } else {
                None
            },
        });
    }

    while let Ok(Some(entry)) = entries.next_entry().await {
        let name = entry.file_name().to_string_lossy().to_string();

        // Skip hidden files if not requested
        if !params.include_hidden && name.starts_with('.') {
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
            get_file_attributes(&entry.path().to_string_lossy(), true)
                .await
                .ok()
        } else {
            None
        };

        results.push(DirEntry {
            name,
            file_type,
            attrs,
        });
    }

    // Sort by name
    results.sort_by(|a, b| a.name.cmp(&b.name));

    Ok(serde_json::to_value(results).unwrap())
}

/// Create a directory
pub async fn create(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
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

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = Path::new(&params.path);

    let result = if params.parents {
        fs::create_dir_all(path).await
    } else {
        fs::create_dir(path).await
    };

    result.map_err(|e| super::file::map_io_error(e, &params.path))?;

    // Set permissions
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(params.mode);
        fs::set_permissions(path, perms)
            .await
            .map_err(|e| super::file::map_io_error(e, &params.path))?;
    }

    Ok(serde_json::json!(true))
}

/// Remove a directory
pub async fn remove(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        /// Remove recursively
        #[serde(default)]
        recursive: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = Path::new(&params.path);

    let result = if params.recursive {
        fs::remove_dir_all(path).await
    } else {
        fs::remove_dir(path).await
    };

    result.map_err(|e| super::file::map_io_error(e, &params.path))?;

    Ok(serde_json::json!(true))
}

/// Get completions for a path prefix
pub async fn completions(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Directory to search in
        directory: String,
        /// Prefix to complete
        prefix: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

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

    Ok(serde_json::to_value(completions).unwrap())
}
