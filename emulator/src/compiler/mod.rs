use std::collections::HashMap;

pub(crate) mod layout;
pub(crate) mod memory;

type Labels = HashMap<String, u64>;

/// Holds informations about the compilation
pub(crate) struct DebugInfo {
    /// Map of labels to addresses
    pub labels: Labels,
}
