//! Structures to represent most of argument combinations

use parse_display::Display;

use self::conversions::ArgKind;

use super::registers::Reg;

use crate::constants as C;

pub use conversions::ArgConversionError;
pub use traits::{ExtractError, ExtractValue, ResolveAddress};

/**
 * First, individual argument types (`Reg` is defined in `crate::runtime::registers::Reg`)
 */

/// An immediate value
#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("{0}")]
pub struct Imm(pub C::Word);

impl Imm {
    /// CPU cycles count to use this value
    pub const fn cost() -> usize {
        0
    }
}

/// A direct memory access
#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("[{0}]")]
pub struct Dir(pub C::Address);

impl Dir {
    /// CPU cycles count to use this value
    pub const fn cost() -> usize {
        1
    }
}

/// An indirect memory access (from a register value)
#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("[{0}]")]
pub struct Ind(pub Reg);

impl Ind {
    /// CPU cycles count to use this value
    pub const fn cost() -> usize {
        1
    }
}

/// An indexed memory access (from a register value and an offset)
#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("[{0}{1:+}]")]
pub struct Idx(pub Reg, pub C::Word);

impl Idx {
    /// CPU cycles count to use this value
    pub const fn cost() -> usize {
        1
    }
}

/**
 * Then define the combination of argument types needed
 */

#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("{0}")]
pub enum ImmRegDirIndIdx {
    Imm(Imm),
    Reg(Reg),
    Dir(Dir),
    Ind(Ind),
    Idx(Idx),
}

impl ImmRegDirIndIdx {
    /// CPU cycles count to use this value
    pub const fn cost(&self) -> usize {
        match self {
            ImmRegDirIndIdx::Imm(_) => Imm::cost(),
            ImmRegDirIndIdx::Reg(_) => Reg::cost(),
            ImmRegDirIndIdx::Dir(_) => Dir::cost(),
            ImmRegDirIndIdx::Ind(_) => Ind::cost(),
            ImmRegDirIndIdx::Idx(_) => Idx::cost(),
        }
    }

    /// Get the kind of this argument
    /// This is used to build nice errors on conversions
    const fn kind(&self) -> conversions::ArgKind {
        match self {
            Self::Imm(_) => ArgKind::Imm,
            Self::Reg(_) => ArgKind::Reg,
            Self::Dir(_) => ArgKind::Dir,
            Self::Ind(_) => ArgKind::Ind,
            Self::Idx(_) => ArgKind::Idx,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("{0}")]
pub enum DirIndIdx {
    Dir(Dir),
    Ind(Ind),
    Idx(Idx),
}

impl DirIndIdx {
    pub const fn cost(&self) -> usize {
        match self {
            DirIndIdx::Dir(_) => Dir::cost(),
            DirIndIdx::Ind(_) => Ind::cost(),
            DirIndIdx::Idx(_) => Idx::cost(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("{0}")]
pub enum RegDirIndIdx {
    Reg(Reg),
    Dir(Dir),
    Ind(Ind),
    Idx(Idx),
}

impl RegDirIndIdx {
    pub const fn cost(&self) -> usize {
        match self {
            RegDirIndIdx::Reg(_) => Reg::cost(),
            RegDirIndIdx::Dir(_) => Dir::cost(),
            RegDirIndIdx::Ind(_) => Ind::cost(),
            RegDirIndIdx::Idx(_) => Idx::cost(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Display)]
#[display("{0}")]
pub enum ImmReg {
    Imm(Imm),
    Reg(Reg),
}

impl ImmReg {
    pub const fn cost(&self) -> usize {
        match self {
            ImmReg::Imm(_) => Imm::cost(),
            ImmReg::Reg(_) => Reg::cost(),
        }
    }
}

/// Traits used to extract values and addresses specified by arguments
mod traits {
    use std::convert::TryFrom;

    use thiserror::Error;

    use super::super::{
        memory::{CellError, MemoryError},
        registers::Reg,
        Cell, Computer, Registers,
    };
    use super::{Dir, DirIndIdx, Idx, Imm, ImmReg, ImmRegDirIndIdx, Ind, RegDirIndIdx};

    use crate::constants as C;

    pub trait ResolveAddress {
        fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError>;
    }

    #[derive(Debug, Error)]
    pub enum ExtractError {
        #[error("cell error: {0}")]
        CellError(#[from] CellError),

        #[error("memory error: {0}")]
        MemoryError(#[from] MemoryError),

        #[error("invalid address: {0}")]
        InvalidAddress(#[from] std::num::TryFromIntError),
    }

    pub trait ExtractValue {
        fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError>;
        fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
            let cell = self.extract_cell(c)?;
            let word = cell.extract_word()?;
            Ok(word)
        }
        fn extract_address(&self, c: &Computer) -> Result<C::Address, ExtractError> {
            let word = self.extract_word(c)?;
            let addr = C::Address::try_from(word)?;
            Ok(addr)
        }
    }

    // Generic implementation of ExtractValue for types that refer to an address
    impl<T: ResolveAddress> ExtractValue for T {
        fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
            let addr = self.resolve_address(&c.registers)?;
            let cell = c.memory.get(addr)?;
            Ok(cell.clone())
        }

        fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
            let addr = self.resolve_address(&c.registers)?;
            let cell = c.memory.get(addr)?;
            Ok(cell.extract_word()?)
        }
    }

    impl ResolveAddress for Ind {
        fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError> {
            // Get the register value
            let cell = c.get(&self.0);
            // and try converting it to an address
            let addr = C::Address::try_from(&cell)?;
            Ok(addr)
        }
    }

    impl ResolveAddress for Dir {
        fn resolve_address(&self, _c: &Registers) -> Result<C::Address, CellError> {
            Ok(self.0)
        }
    }

    impl ResolveAddress for Idx {
        fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError> {
            // Get the register value
            let cell = c.get(&self.0);
            // and try converting it to a word
            let addr = C::Word::try_from(&cell)?;
            // add the offset
            let addr = addr + self.1;
            // and convert it to an address
            let addr = C::Address::try_from(&addr.into())?;
            Ok(addr)
        }
    }

    impl ResolveAddress for DirIndIdx {
        fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError> {
            match self {
                DirIndIdx::Dir(a) => a.resolve_address(c),
                DirIndIdx::Ind(a) => a.resolve_address(c),
                DirIndIdx::Idx(a) => a.resolve_address(c),
            }
        }
    }

    impl ExtractValue for ImmRegDirIndIdx {
        fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
            match self {
                ImmRegDirIndIdx::Imm(a) => a.extract_cell(c),
                ImmRegDirIndIdx::Reg(a) => a.extract_cell(c),
                ImmRegDirIndIdx::Dir(a) => a.extract_cell(c),
                ImmRegDirIndIdx::Ind(a) => a.extract_cell(c),
                ImmRegDirIndIdx::Idx(a) => a.extract_cell(c),
            }
        }

        fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
            match self {
                ImmRegDirIndIdx::Imm(a) => a.extract_word(c),
                ImmRegDirIndIdx::Reg(a) => a.extract_word(c),
                ImmRegDirIndIdx::Dir(a) => a.extract_word(c),
                ImmRegDirIndIdx::Ind(a) => a.extract_word(c),
                ImmRegDirIndIdx::Idx(a) => a.extract_word(c),
            }
        }
    }

    impl ExtractValue for Imm {
        fn extract_cell(&self, _c: &Computer) -> Result<Cell, ExtractError> {
            Ok(Cell::Word(self.0))
        }

        fn extract_word(&self, _c: &Computer) -> Result<C::Word, ExtractError> {
            Ok(self.0)
        }
    }

    impl ExtractValue for Reg {
        fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
            Ok(c.registers.get(self))
        }

        fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
            Ok(c.registers.get_word(*self)?)
        }
    }

    impl ExtractValue for ImmReg {
        fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
            match self {
                ImmReg::Imm(a) => a.extract_cell(c),
                ImmReg::Reg(a) => a.extract_cell(c),
            }
        }

        fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
            match self {
                ImmReg::Imm(a) => a.extract_word(c),
                ImmReg::Reg(a) => a.extract_word(c),
            }
        }
    }

    impl ExtractValue for RegDirIndIdx {
        fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
            match self {
                RegDirIndIdx::Reg(a) => a.extract_cell(c),
                RegDirIndIdx::Dir(a) => a.extract_cell(c),
                RegDirIndIdx::Ind(a) => a.extract_cell(c),
                RegDirIndIdx::Idx(a) => a.extract_cell(c),
            }
        }
    }
}

/// Conversions between argument types
mod conversions {
    use std::convert::TryFrom;

    use parse_display::Display;
    use thiserror::Error;

    use super::{DirIndIdx, ImmReg, ImmRegDirIndIdx, Reg, RegDirIndIdx};

    /// Valid argument kinds
    #[derive(PartialEq, Eq, Clone, Debug, Display)]
    #[display(style = "lowercase")]
    pub enum ArgKind {
        Imm,
        Reg,
        Ind,
        Dir,
        Idx,
    }

    /// Type alias used in argument conversion errors
    type K = ArgKind;

    /// A list of argument kinds, wrap for display purposes
    #[derive(Debug)]
    struct ArgKinds(Vec<ArgKind>);

    impl<T: Into<Vec<ArgKind>>> From<T> for ArgKinds {
        fn from(l: T) -> Self {
            Self(l.into())
        }
    }

    impl std::fmt::Display for ArgKinds {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for (index, entry) in self.0.iter().enumerate() {
                if index != 0 {
                    write!(f, "/")?;
                }
                write!(f, "{entry}")?;
            }
            Ok(())
        }
    }

    /// Error used when converting from an error type to another one
    #[derive(Error, Debug)]
    #[error("invalid argument type '{got}', expected '{expected}'")]
    pub struct ArgConversionError {
        /// The list of kind of arguments that were accepted
        expected: ArgKinds,
        got: ArgKind,
    }

    impl TryFrom<ImmRegDirIndIdx> for ImmReg {
        type Error = ArgConversionError;

        fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
            match value {
                ImmRegDirIndIdx::Imm(a) => Ok(Self::Imm(a)),
                ImmRegDirIndIdx::Reg(a) => Ok(Self::Reg(a)),
                other => Err(ArgConversionError {
                    expected: [K::Imm, K::Reg].into(),
                    got: other.kind(),
                }),
            }
        }
    }

    impl TryFrom<ImmRegDirIndIdx> for DirIndIdx {
        type Error = ArgConversionError;

        fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
            match value {
                ImmRegDirIndIdx::Dir(a) => Ok(Self::Dir(a)),
                ImmRegDirIndIdx::Ind(a) => Ok(Self::Ind(a)),
                ImmRegDirIndIdx::Idx(a) => Ok(Self::Idx(a)),
                other => Err(ArgConversionError {
                    expected: [K::Dir, K::Ind, K::Idx].into(),
                    got: other.kind(),
                }),
            }
        }
    }

    impl TryFrom<ImmRegDirIndIdx> for Reg {
        type Error = ArgConversionError;

        fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
            match value {
                ImmRegDirIndIdx::Reg(a) => Ok(a),
                other => Err(ArgConversionError {
                    expected: [K::Reg].into(),
                    got: other.kind(),
                }),
            }
        }
    }

    impl TryFrom<ImmRegDirIndIdx> for RegDirIndIdx {
        type Error = ArgConversionError;

        fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
            match value {
                ImmRegDirIndIdx::Reg(a) => Ok(Self::Reg(a)),
                ImmRegDirIndIdx::Dir(a) => Ok(Self::Dir(a)),
                ImmRegDirIndIdx::Ind(a) => Ok(Self::Ind(a)),
                ImmRegDirIndIdx::Idx(a) => Ok(Self::Idx(a)),
                other @ ImmRegDirIndIdx::Imm(_) => Err(ArgConversionError {
                    expected: [K::Reg, K::Dir, K::Ind, K::Idx].into(),
                    got: other.kind(),
                }),
            }
        }
    }

    impl TryFrom<RegDirIndIdx> for DirIndIdx {
        type Error = ArgConversionError;

        fn try_from(value: RegDirIndIdx) -> Result<Self, Self::Error> {
            match value {
                RegDirIndIdx::Dir(a) => Ok(Self::Dir(a)),
                RegDirIndIdx::Ind(a) => Ok(Self::Ind(a)),
                RegDirIndIdx::Idx(a) => Ok(Self::Idx(a)),
                RegDirIndIdx::Reg(_) => Err(ArgConversionError {
                    expected: [K::Dir, K::Ind, K::Idx].into(),
                    got: K::Reg,
                }),
            }
        }
    }
}
