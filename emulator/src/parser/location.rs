use nom::Offset;
use parse_display::Display;

#[derive(Clone, Debug, PartialEq, Display)]
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
pub struct RelativeLocation {
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
    pub fn into_absolute(self, parent: &AbsoluteLocation) -> AbsoluteLocation {
        self.to_absolute(parent)
    }

    pub(crate) fn to_absolute(&self, parent: &AbsoluteLocation) -> AbsoluteLocation {
        AbsoluteLocation {
            offset: parent.offset + self.offset,
            length: self.length,
        }
    }
}

impl std::fmt::Display for RelativeLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.offset, self.offset + self.length)
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
pub struct AbsoluteLocation {
    offset: usize,
    length: usize,
}

impl From<(usize, usize)> for AbsoluteLocation {
    fn from((offset, length): (usize, usize)) -> Self {
        AbsoluteLocation { offset, length }
    }
}

impl std::fmt::Display for AbsoluteLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.offset, self.offset + self.length)
    }
}

impl AbsoluteLocation {
    pub fn to_line_aware(&self, lines: &Lines) -> LineAwareLocation {
        // TODO: this is very inefficient
        // TODO: this can also crash, it should return a Result instead
        let start = self.offset;
        let end = self.offset + self.length;
        let (line, offset) = lines
            .0
            .iter()
            .map(|(offset, _)| *offset)
            .filter(|offset| *offset <= start)
            .enumerate()
            .last()
            .unwrap_or((0, 0));
        let start_line = line + 1;
        let start_col = start - offset + 1; // TODO: make this relative to chars, not bytes

        let (line, offset) = lines
            .0
            .iter()
            .map(|(offset, _)| *offset)
            .filter(|offset| *offset <= end)
            .enumerate()
            .last()
            .unwrap_or((0, 0));
        let end_line = line + 1;
        let end_col = end - offset + 1; // TODO: make this relative to chars, not bytes

        LineAwareLocation {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

pub struct Lines(Vec<(usize, String)>);

impl Lines {
    pub fn new(input: &str) -> Self {
        Self(
            input
                .lines()
                .map(|line| (input.offset(line), line.to_string()))
                .collect(),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct LineAwareLocation {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
}

impl std::fmt::Display for LineAwareLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start_line == self.end_line {
            write!(
                f,
                "{}:{}..{}",
                self.start_line, self.start_col, self.end_col
            )
        } else {
            write!(
                f,
                "{}:{}..{}:{}",
                self.start_line, self.start_col, self.end_line, self.end_col,
            )
        }
    }
}
