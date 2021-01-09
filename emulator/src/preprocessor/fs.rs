use std::{
    collections::HashMap,
    io::{Cursor, Read},
    path::PathBuf,
};

trait Filesystem {
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

struct InMemoryFilesystem {
    files: HashMap<PathBuf, String>,
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
    working_directory: PathBuf,
}

impl Filesystem for NativeFilesystem {
    type File = std::fs::File;

    fn open(&self, path: &PathBuf) -> std::io::Result<Self::File> {
        std::fs::File::open(path)
    }

    fn root(&self) -> PathBuf {
        self.working_directory.clone()
    }
}
