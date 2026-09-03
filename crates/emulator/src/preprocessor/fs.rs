use std::collections::HashMap;

use camino::{Utf8Component, Utf8Path, Utf8PathBuf};

/// Resolve `.` and `..` in `path` lexically, without consulting the
/// filesystem, so that two spellings of the same file compare equal.
fn normalize(path: &Utf8Path) -> Utf8PathBuf {
    let mut out = Utf8PathBuf::new();
    for component in path.components() {
        match component {
            Utf8Component::CurDir => {}
            Utf8Component::ParentDir => {
                if !out.pop() {
                    out.push(component);
                }
            }
            other => out.push(other),
        }
    }
    out
}

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

    /// Get the absolute path of a file relative to the root.
    ///
    /// The result is lexically normalized: the include stack compares paths as
    /// strings, so `b/../c.S` and `c.S` have to resolve to the same one.
    fn relative(&self, sibling: Option<&Utf8Path>, path: &Utf8Path) -> Utf8PathBuf {
        let joined = sibling
            .and_then(Utf8Path::parent)
            .unwrap_or(self.root()) // Default to the "root" path
            .join(path); // And join relative to that
        normalize(&joined)
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
                .map_err(std::io::Error::other)?,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn relative_normalizes_dot_and_dotdot() {
        let fs = InMemoryFilesystem::new([]);
        let sibling = Utf8Path::new("/dir/c.S");
        assert_eq!(
            fs.relative(Some(sibling), Utf8Path::new("b/../c.S")),
            Utf8PathBuf::from("/dir/c.S")
        );
        assert_eq!(
            fs.relative(Some(sibling), Utf8Path::new("./c.S")),
            Utf8PathBuf::from("/dir/c.S")
        );
        assert_eq!(
            fs.relative(Some(sibling), Utf8Path::new("../c.S")),
            Utf8PathBuf::from("/c.S")
        );
    }
}
