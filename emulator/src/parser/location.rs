use nom::Offset;
use parse_display::Display;

#[derive(Clone, Debug, PartialEq, Eq, Display)]
#[display("{inner}", bound(T))]
pub struct Located<T, L> {
    pub inner: T,
    pub(crate) location: L,
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

pub trait Locatable: Sized {
    fn with_location<L, L1: Into<L>>(self, location: L1) -> Located<Self, L> {
        Located {
            inner: self,
            location: location.into(),
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

impl RelativeLocation {
    #[must_use]
    pub fn into_absolute(self, parent: &AbsoluteLocation) -> AbsoluteLocation {
        self.to_absolute(parent)
    }

    pub(crate) fn to_absolute(&self, parent: &AbsoluteLocation) -> AbsoluteLocation {
        AbsoluteLocation {
            offset: parent.offset + self.offset,
            length: self.length,
            file: (),
        }
    }
}

impl std::fmt::Display for RelativeLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.offset, self.offset + self.length)
    }
}

impl<T> Located<T, RelativeLocation> {
    pub fn into_absolute<O, M>(
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

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AbsoluteLocation<File = ()> {
    pub offset: usize,
    pub length: usize,
    pub file: File,
}

impl<F: Default> From<(usize, usize)> for AbsoluteLocation<F> {
    fn from((offset, length): (usize, usize)) -> Self {
        AbsoluteLocation {
            offset,
            length,
            file: F::default(),
        }
    }
}

impl std::fmt::Display for AbsoluteLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.offset, self.offset + self.length)
    }
}

pub trait MapLocation<Parent> {
    type Mapped;

    fn map_location(self, parent: &Parent) -> Self::Mapped;
}

impl<P, L, T> MapLocation<P> for Located<T, L>
where
    T: MapLocation<L::Mapped>,
    L: MapLocation<P>,
{
    type Mapped = Located<T::Mapped, L::Mapped>;

    fn map_location(self, parent: &P) -> Self::Mapped {
        let location = self.location.map_location(parent);
        let inner = self.inner.map_location(&location);

        Located { inner, location }
    }
}

impl<P, T> MapLocation<P> for Box<T>
where
    T: MapLocation<P>,
{
    type Mapped = Box<T::Mapped>;

    fn map_location(self, parent: &P) -> Self::Mapped {
        Box::new((*self).map_location(parent))
    }
}

impl<P, T> MapLocation<P> for Option<T>
where
    T: MapLocation<P>,
{
    type Mapped = Option<T::Mapped>;

    fn map_location(self, parent: &P) -> Self::Mapped {
        self.map(|inner| inner.map_location(parent))
    }
}

impl<P, T> MapLocation<P> for Vec<T>
where
    T: MapLocation<P>,
{
    type Mapped = Vec<T::Mapped>;

    fn map_location(self, parent: &P) -> Self::Mapped {
        self.into_iter()
            .map(|item| item.map_location(parent))
            .collect()
    }
}

impl<F> MapLocation<AbsoluteLocation<F>> for RelativeLocation
where
    F: Clone,
{
    type Mapped = AbsoluteLocation<F>;

    fn map_location(self, parent: &AbsoluteLocation<F>) -> Self::Mapped {
        AbsoluteLocation {
            offset: parent.offset + self.offset,
            length: self.length,
            file: parent.file.clone(),
        }
    }
}

impl MapLocation<()> for AbsoluteLocation {
    type Mapped = ();
    fn map_location(self, _parent: &()) -> Self::Mapped {}
}

impl MapLocation<()> for RelativeLocation {
    type Mapped = ();
    fn map_location(self, _parent: &()) -> Self::Mapped {}
}

impl<T, L> Located<T, L> {
    pub fn map_location_only<P>(self, parent: &P) -> Located<T, L::Mapped>
    where
        L: MapLocation<P>,
    {
        let location = self.location.map_location(parent);
        Located {
            location,
            inner: self.inner,
        }
    }
}
