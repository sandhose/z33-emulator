use std::ops::Range;

use nom::Offset;
use parse_display::Display;

#[derive(Clone, Debug, PartialEq, Eq, Display)]
#[display("{inner}", bound(T))]
pub struct Located<T> {
    pub inner: T,
    pub(crate) location: Range<usize>,
}

impl<T> Located<T> {
    pub(crate) fn offset(self, offset: usize) -> Self {
        Located {
            inner: self.inner,
            location: Range {
                start: offset + self.location.start,
                end: offset + self.location.end,
            },
        }
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

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct RelativeLocation {
    offset: usize,
    length: usize,
}

impl From<()> for RelativeLocation {
    fn from((): ()) -> Self {
        Self::default()
    }
}

impl From<(usize, usize)> for RelativeLocation {
    fn from((offset, length): (usize, usize)) -> Self {
        RelativeLocation { offset, length }
    }
}

impl<T> From<(T, T, T)> for RelativeLocation
where
    T: Offset,
{
    fn from((full, start, end): (T, T, T)) -> Self {
        let offset = full.offset(&start);
        let length = start.offset(&end);
        RelativeLocation { offset, length }
    }
}

impl std::fmt::Display for RelativeLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.offset, self.offset + self.length)
    }
}
