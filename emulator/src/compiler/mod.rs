use std::collections::HashMap;

pub mod layout;
pub mod memory;

type Labels = HashMap<String, u64>;

/// Holds informations about the compilation
pub struct DebugInfo {
    /// Map of labels to addresses
    pub labels: Labels,
}
