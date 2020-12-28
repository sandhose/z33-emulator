use nom::Offset;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Located<T, L> {
    pub inner: T,
    location: L,
}

impl<T> Located<T, RelativeLocation> {
    pub(crate) fn offset(self, offset: usize) -> Self {
        Located {
            inner: self.inner,
            location: RelativeLocation {
                offset: offset + self.location.offset,
                length: self.location.length,
            },
        }
    }
}

pub(crate) trait Locatable: Sized {
    fn with_location<L, L1: Into<L>>(self, location: L1) -> Located<Self, L> {
        Located {
            inner: self,
            location: location.into(),
        }
    }
}

impl<T> Locatable for T {}

#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) struct RelativeLocation {
    offset: usize,
    length: usize,
}

impl From<()> for RelativeLocation {
    fn from(_: ()) -> Self {
        Default::default()
    }
}

impl From<(usize, usize)> for RelativeLocation {
    fn from((offset, length): (usize, usize)) -> Self {
        RelativeLocation { offset, length }
    }
}

impl<'a> From<(&'a str, &'a str, &'a str)> for RelativeLocation {
    fn from((full, start, end): (&'a str, &'a str, &'a str)) -> Self {
        let offset = full.offset(start);
        let length = start.offset(end);
        RelativeLocation { offset, length }
    }
}

impl RelativeLocation {
    pub(crate) fn to_absolute(&self, parent: &AbsoluteLocation) -> AbsoluteLocation {
        AbsoluteLocation {
            offset: parent.offset + self.offset,
            length: self.length,
        }
    }
}

impl<T> Located<T, RelativeLocation> {
    pub(crate) fn into_absolute<O, M>(
        self,
        parent: &AbsoluteLocation,
        mapper: M,
    ) -> Located<O, AbsoluteLocation>
    where
        M: Fn(T, &AbsoluteLocation) -> O,
    {
        let location = self.location.to_absolute(parent);
        let inner = mapper(self.inner, &location);
        Located { inner, location }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) struct AbsoluteLocation {
    offset: usize,
    length: usize,
}

impl From<(usize, usize)> for AbsoluteLocation {
    fn from((offset, length): (usize, usize)) -> Self {
        AbsoluteLocation { offset, length }
    }
}
