use std::{
    collections::HashMap,
    io::{Cursor, Read},
    path::{Path, PathBuf},
};

/// Abstraction over a filesystem
pub trait Filesystem {
    /// The type of file that can be read
    type File: Read;

    /// Open a file
    ///
    /// # Errors
    ///
    /// This function will return an error if the file cannot be opened
    fn open(&self, path: &Path) -> std::io::Result<Self::File>;

    /// Get the root path of the filesystem
    fn root(&self) -> PathBuf {
        PathBuf::new()
    }

    /// Get the absolute path of a file relative to the root
    fn relative(&self, sibling: Option<&Path>, path: &Path) -> PathBuf {
        sibling
            .and_then(std::path::Path::parent)
            .map_or_else(|| self.root(), ToOwned::to_owned) // Default to the "root" path
            .join(path) // And join relative to that
    }
}

#[derive(Debug)]
pub struct InMemoryFilesystem {
    files: HashMap<PathBuf, String>,
}

impl InMemoryFilesystem {
    #[must_use]
    pub const fn new(files: HashMap<PathBuf, String>) -> Self {
        InMemoryFilesystem { files }
    }
}

impl Filesystem for InMemoryFilesystem {
    type File = Cursor<String>;

    fn open(&self, path: &Path) -> std::io::Result<Self::File> {
        self.files
            .get(path)
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "file not found"))
            .map(|content| Cursor::new(content.clone()))
    }
}

pub struct NativeFilesystem {
    root: PathBuf,
}

impl NativeFilesystem {
    /// Create a new filesystem from the current directory
    ///
    /// # Errors
    ///
    /// This function will return an error if the current directory cannot be read
    pub fn from_env() -> std::io::Result<Self> {
        Ok(NativeFilesystem {
            root: std::env::current_dir()?,
        })
    }
}

impl Filesystem for NativeFilesystem {
    type File = std::fs::File;

    fn open(&self, path: &Path) -> std::io::Result<Self::File> {
        std::fs::File::open(path)
    }

    fn root(&self) -> PathBuf {
        self.root.clone()
    }
}
