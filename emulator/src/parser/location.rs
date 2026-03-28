use std::ops::Range;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Located<T> {
    pub inner: T,
    pub(crate) location: Range<usize>,
}

impl<T> std::fmt::Display for Located<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

pub trait Locatable: Sized {
    fn with_location(self, location: Range<usize>) -> Located<Self> {
        Located {
            inner: self,
            location,
        }
    }
}

impl<T> Locatable for T {}
