use std::collections::HashMap;

use camino::{Utf8Path, Utf8PathBuf};

/// Abstraction over a filesystem
pub trait Filesystem {
    /// Read the content of a file
    ///
    /// # Errors
    ///
    /// This function will return an error if the file cannot be opened or read
    fn read(&self, path: &Utf8Path) -> std::io::Result<String>;

    /// Get the root path of the filesystem
    fn root(&self) -> &Utf8Path {
        Utf8Path::new("")
    }

    /// Get the absolute path of a file relative to the root
    fn relative(&self, sibling: Option<&Utf8Path>, path: &Utf8Path) -> Utf8PathBuf {
        sibling
            .and_then(Utf8Path::parent)
            .unwrap_or(self.root()) // Default to the "root" path
            .join(path) // And join relative to that
    }
}

#[derive(Debug)]
pub struct InMemoryFilesystem {
    files: HashMap<Utf8PathBuf, String>,
}

impl InMemoryFilesystem {
    #[must_use]
    pub fn new<T: Into<HashMap<Utf8PathBuf, String>>>(files: T) -> Self {
        InMemoryFilesystem {
            files: files.into(),
        }
    }
}

impl Filesystem for InMemoryFilesystem {
    fn read(&self, path: &Utf8Path) -> std::io::Result<String> {
        self.files
            .get(path)
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "file not found"))
            .cloned()
    }
}

pub struct NativeFilesystem {
    root: Utf8PathBuf,
}

impl NativeFilesystem {
    /// Create a new filesystem from the current directory
    ///
    /// # Errors
    ///
    /// This function will return an error if the current directory cannot be
    /// read
    pub fn from_env() -> std::io::Result<Self> {
        Ok(NativeFilesystem {
            root: std::env::current_dir()?
                .try_into()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?,
        })
    }
}

impl Filesystem for NativeFilesystem {
    fn read(&self, path: &Utf8Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn root(&self) -> &Utf8Path {
        &self.root
    }
}
