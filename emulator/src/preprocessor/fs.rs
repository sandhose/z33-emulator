use std::{
    collections::HashMap,
    io::{Cursor, Read},
    path::PathBuf,
};

pub(crate) trait Filesystem {
    type File: Read;

    fn open(&self, path: &PathBuf) -> std::io::Result<Self::File>;

    fn root(&self) -> PathBuf {
        PathBuf::new()
    }

    fn relative(&self, sibling: Option<&PathBuf>, path: &PathBuf) -> PathBuf {
        sibling
            .and_then(|s| s.parent()) // Get the parent of the sibling
            .map(ToOwned::to_owned) // PathBuf::parent outputs a reference, make it owned again
            .unwrap_or_else(|| self.root()) // Default to the "root" path
            .join(path) // And join relative to that
    }
}

pub(crate) struct InMemoryFilesystem {
    files: HashMap<PathBuf, String>,
}

impl InMemoryFilesystem {
    pub(crate) const fn new(files: HashMap<PathBuf, String>) -> Self {
        InMemoryFilesystem { files }
    }
}

impl Filesystem for InMemoryFilesystem {
    type File = Cursor<String>;

    fn open(&self, path: &PathBuf) -> std::io::Result<Self::File> {
        self.files
            .get(path)
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "file not found"))
            .map(|content| Cursor::new(content.clone()))
    }
}

struct NativeFilesystem {
    root: PathBuf,
}

impl NativeFilesystem {
    pub(crate) fn from_env() -> std::io::Result<Self> {
        Ok(NativeFilesystem {
            root: std::env::current_dir()?,
        })
    }
}

impl Filesystem for NativeFilesystem {
    type File = std::fs::File;

    fn open(&self, path: &PathBuf) -> std::io::Result<Self::File> {
        std::fs::File::open(path)
    }

    fn root(&self) -> PathBuf {
        self.root.clone()
    }
}
