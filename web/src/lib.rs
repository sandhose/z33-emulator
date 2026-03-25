use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::ops::Range;
use std::rc::Rc;

use camino::Utf8PathBuf;
use miette::JSONReportHandler;
use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::*;
use z33_emulator::compiler::{check as compiler_check, compile};
use z33_emulator::constants::Address;
use z33_emulator::preprocessor::{
    InMemoryFilesystem, PreprocessorError, ReferencingSourceMap, Workspace,
};
use z33_emulator::runtime::{Memory, ProcessorError};

/// A wrapper diagnostic that bundles multiple parse errors.
/// The first error is rendered as the main diagnostic; the rest appear as
/// `related`, so the JSON handler (and thus the web app) sees all of them.
struct ParseErrors {
    main: miette::Report,
    related: Vec<miette::Report>,
}

impl std::fmt::Debug for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.main, f)
    }
}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.main, f)
    }
}

impl std::error::Error for ParseErrors {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.main.source()
    }
}

impl miette::Diagnostic for ParseErrors {
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.main.source_code()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.main.labels()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.main.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.main.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.main.url()
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.main.code()
    }

    fn related<'a>(
        &'a self,
    ) -> Option<Box<dyn Iterator<Item = &'a dyn miette::Diagnostic> + 'a>> {
        // Chain the main report's related diagnostics with our extra ones
        let main_related = self
            .main
            .related()
            .into_iter()
            .flatten();
        let extra = self
            .related
            .iter()
            .map(|r| r.as_ref() as &dyn miette::Diagnostic);
        Some(Box::new(main_related.chain(extra)))
    }
}

#[wasm_bindgen(start)]
fn start() {
    console_error_panic_hook::set_once();
    tracing_wasm::set_as_global_default();
    miette::set_hook(Box::new(|_| Box::new(JSONReportHandler))).unwrap();
}

#[wasm_bindgen]
pub struct InMemoryPreprocessor {
    preprocessor: Workspace,
    files: HashMap<String, String>,
}

#[derive(Default, Deserialize, Tsify)]
#[tsify(from_wasm_abi)]
pub struct InputFiles(HashMap<Utf8PathBuf, String>);

#[wasm_bindgen]
pub struct CompilationResult {
    program: Option<Program>,
    preprocessor: Option<miette::Report>,
    compilation: Option<miette::Report>,
}

impl CompilationResult {
    fn preprocessor_error(v: PreprocessorError) -> Self {
        Self {
            program: None,
            preprocessor: Some(miette::Report::new(v)),
            compilation: None,
        }
    }

    fn compilation_error(v: miette::Report) -> Self {
        Self {
            program: None,
            preprocessor: None,
            compilation: Some(v),
        }
    }

    fn new(v: Program) -> Self {
        Self {
            program: Some(v),
            preprocessor: None,
            compilation: None,
        }
    }
}

#[wasm_bindgen]
impl CompilationResult {
    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn program(&self) -> Option<Program> {
        self.program.clone()
    }

    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn report(&self) -> Option<String> {
        if let Some(report) = &self.preprocessor {
            return Some(format!("{report:?}"));
        }

        if let Some(report) = &self.compilation {
            return Some(format!("{report:?}"));
        }

        None
    }
}

#[wasm_bindgen]
impl InMemoryPreprocessor {
    #[wasm_bindgen(constructor)]
    #[must_use]
    pub fn new(files: InputFiles, entrypoint: String) -> Self {
        let files_map: HashMap<String, String> = files
            .0
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        let fs = InMemoryFilesystem::new(files.0);
        let entrypoint = Utf8PathBuf::from(entrypoint);
        let preprocessor = Workspace::new(&fs, &entrypoint);
        Self {
            preprocessor,
            files: files_map,
        }
    }

    /// Compile the given file
    ///
    /// # Errors
    ///
    /// Returns an error if the file could not be preprocessed or compiled.
    #[allow(clippy::missing_panics_doc)]
    pub fn compile(&mut self) -> CompilationResult {
        let (source_map, source) = match self.preprocessor.preprocess() {
            Ok(o) => o,
            Err(e) => return CompilationResult::preprocessor_error(e),
        };
        let source_str = &source;
        let result = z33_emulator::parser::parse(source_str);

        let has_errors = result
            .diagnostics
            .iter()
            .any(|d| d.severity == z33_emulator::parser::DiagnosticSeverity::Error);

        if has_errors {
            // Convert each parse diagnostic into a miette report with source
            // location mapped back to the original file.  The first one
            // becomes the top-level report; the rest are attached as `related`
            // so the web app creates a Monaco marker for each.
            let mut reports: Vec<miette::Report> = result
                .diagnostics
                .iter()
                .filter(|d| d.severity == z33_emulator::parser::DiagnosticSeverity::Error)
                .filter_map(|diag| {
                    let span = source_map.find(diag.span.start)?;
                    let labels =
                        vec![miette::LabeledSpan::underline(span.span.clone())];
                    Some(
                        miette::miette!(labels = labels, "{}", diag.message)
                            .with_source_code(span.source.clone()),
                    )
                })
                .collect();

            let report = if reports.is_empty() {
                miette::miette!("Failed to parse program")
            } else if reports.len() == 1 {
                reports.remove(0)
            } else {
                // Bundle: first error is the root, the rest are `related`
                let first = reports.remove(0);
                miette::Report::new(ParseErrors {
                    main: first,
                    related: reports,
                })
            };

            return CompilationResult::compilation_error(report);
        }

        CompilationResult::new(Program {
            source_map: source_map.into(),
            files: self.files.clone(),
            program: result.program,
        })
    }
}

#[derive(Serialize, Tsify, Clone)]
#[tsify(into_wasm_abi)]
pub struct SourceLocation {
    pub file: String,
    pub span: (usize, usize),
}

/// Compose the debug info source map (address → byte range in preprocessor
/// output) with the preprocessor source map (byte offset → original file
/// location) to produce a map of address → original source location.
fn compose_source_maps(
    debug_source_map: &BTreeMap<Address, Range<usize>>,
    preprocessor_source_map: &ReferencingSourceMap,
) -> BTreeMap<Address, SourceLocation> {
    debug_source_map
        .iter()
        .filter_map(|(&address, range)| {
            let (chunk_key, span) = preprocessor_source_map.find_with_key(range.start)?;
            let start = span.span.start + (range.start - chunk_key);
            let end = span.span.start + (range.end - chunk_key);
            Some((
                address,
                SourceLocation {
                    file: span.name.clone(),
                    span: (start, end),
                },
            ))
        })
        .collect()
}

fn compiler_error_to_report(
    error: &z33_emulator::compiler::CompilationError,
    source_map: &ReferencingSourceMap,
    files: &HashMap<String, String>,
) -> miette::Report {
    use std::error::Error as StdError;

    use z33_emulator::compiler::CompilationError;

    // Build a full message by walking the error source chain
    let mut msg = error.to_string();
    let mut cause: Option<&dyn StdError> = StdError::source(error);
    while let Some(c) = cause {
        msg.push_str(": ");
        msg.push_str(&c.to_string());
        cause = c.source();
    }

    // Extract the byte range in the preprocessed source, if available
    let location: Option<std::ops::Range<usize>> = match error {
        CompilationError::MemoryFill(e) => Some(e.location().clone()),
        CompilationError::MemoryLayout(e) => e.location().cloned(),
        CompilationError::UnknownEntrypoint(_) | CompilationError::HasParseErrors => None,
    };

    if let Some(loc) = location {
        if let Some((chunk_key, span_info)) = source_map.find_with_key(loc.start) {
            if let Some(content) = files.get(&span_info.name) {
                let start = span_info.span.start + (loc.start - chunk_key);
                let end = span_info.span.start + (loc.end - chunk_key);
                let named_source =
                    miette::NamedSource::new(span_info.name.clone(), content.clone());
                return miette::miette!(
                    labels = vec![miette::LabeledSpan::underline(start..end)],
                    "{msg}"
                )
                .with_source_code(named_source);
            }
        }
    }

    miette::miette!("{msg}")
}

#[wasm_bindgen]
#[derive(Clone)]
#[allow(clippy::struct_field_names)]
pub struct Program {
    source_map: ReferencingSourceMap,
    files: HashMap<String, String>,
    program: z33_emulator::parser::location::Located<z33_emulator::parser::Program>,
}

#[wasm_bindgen]
impl Program {
    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn ast(&self) -> String {
        let ast = self.program.to_node();
        format!("{ast}")
    }

    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn labels(&self) -> Labels {
        Labels(
            self.program
                .inner
                .labels()
                .into_iter()
                .map(ToOwned::to_owned)
                .collect(),
        )
    }

    /// Check whether the program can be assembled (layout + fill phases).
    ///
    /// Returns a JSON-formatted miette report string on failure, or `None` on
    /// success.
    #[wasm_bindgen]
    #[must_use]
    pub fn check(&self) -> Option<String> {
        match compiler_check(&self.program.inner) {
            Ok(()) => None,
            Err(ref e) => {
                let report = compiler_error_to_report(e, &self.source_map, &self.files);
                Some(format!("{report:?}"))
            }
        }
    }

    /// Compile the program at the given entrypoint
    ///
    /// # Errors
    ///
    /// Returns an error if the program could not be compiled.
    #[wasm_bindgen]
    pub fn compile(&self, entrypoint: &str) -> Result<Computer, JsValue> {
        tracing::info!("Compiling");
        let (computer, debug_info) =
            compile(&self.program.inner, entrypoint).map_err(|e| format!("{e}"))?;

        let registers = Observable::new(Registers {
            a: Cell::from_runtime_cell(&computer.registers.a),
            b: Cell::from_runtime_cell(&computer.registers.b),
            pc: computer.registers.pc,
            sp: computer.registers.sp,
            sr: computer.registers.sr.bits(),
        });

        let cycles = Observable::new(Cycles(computer.cycles));

        tracing::info!("Cloning");
        let memory = computer.memory.clone();
        tracing::info!("Cloned");
        let memory = MemoryObserver::new(memory);

        let source_map = compose_source_maps(&debug_info.source_map, &self.source_map);

        tracing::info!("Compiled");

        Ok(Computer {
            debug_info,
            source_map,
            inner: computer,
            cycles,
            registers,
            memory,
        })
    }
}

#[derive(Serialize, Tsify, Clone, Copy, PartialEq, Eq)]
#[tsify(into_wasm_abi)]
pub struct Cycles(usize);

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Labels(Vec<String>);

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct SourceMap(BTreeMap<u32, SourceLocation>);

#[wasm_bindgen]
pub struct Computer {
    debug_info: z33_emulator::compiler::DebugInfo,
    source_map: BTreeMap<Address, SourceLocation>,
    inner: z33_emulator::runtime::Computer,
    cycles: Observable<Cycles>,
    registers: Observable<Registers>,
    memory: MemoryObserver,
}

#[derive(Serialize, Tsify, PartialEq, Clone)]
#[serde(tag = "type", rename_all = "lowercase")]
#[tsify(into_wasm_abi)]
pub enum Cell {
    Instruction { instruction: String },
    Word { word: i64 },
    Empty,
}

impl Cell {
    #[must_use]
    fn from_runtime_cell(cell: &z33_emulator::runtime::Cell) -> Self {
        match cell {
            z33_emulator::runtime::Cell::Instruction(s) => Self::Instruction {
                instruction: s.to_string(),
            },
            z33_emulator::runtime::Cell::Word(w) => Self::Word { word: *w },
            z33_emulator::runtime::Cell::Empty => Self::Empty,
        }
    }
}

#[derive(Serialize, Tsify, PartialEq, Clone)]
#[tsify(into_wasm_abi)]
pub struct Registers {
    a: Cell,
    b: Cell,
    pc: u32,
    sp: u32,
    sr: i64,
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "(registers: Registers) => void")]
    pub type RegistersCallback;

    #[wasm_bindgen(typescript_type = "(cycles: Cycles) => void")]
    pub type CyclesCallback;

    #[wasm_bindgen(typescript_type = "(cell: Cell) => void")]
    pub type MemoryCallback;

    #[wasm_bindgen(typescript_type = "() => void")]
    pub type Unsubscribe;
}

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct LabelsWithAddresses(BTreeMap<String, u32>);

#[wasm_bindgen]
impl Computer {
    /// Run a single step of the computer
    ///
    /// # Errors
    ///
    /// Returns an error if the step could not be executed.
    pub fn step(&mut self) -> Result<bool, JsValue> {
        let res = match self.inner.step() {
            Ok(()) => Ok(false),
            Err(ProcessorError::Reset) => Ok(true),
            Err(e) => Err(format!("{e}").into()),
        };

        let registers = Registers {
            a: Cell::from_runtime_cell(&self.inner.registers.a),
            b: Cell::from_runtime_cell(&self.inner.registers.b),
            pc: self.inner.registers.pc,
            sp: self.inner.registers.sp,
            sr: self.inner.registers.sr.bits(),
        };
        self.registers.set(registers);
        self.cycles.set(Cycles(self.inner.cycles));
        self.memory.set(self.inner.memory.clone());

        res
    }

    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn labels(&self) -> LabelsWithAddresses {
        LabelsWithAddresses(self.debug_info.labels.clone())
    }

    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn source_map(&self) -> SourceMap {
        SourceMap(self.source_map.clone())
    }

    #[must_use]
    pub fn cycles(&self) -> <Cycles as Tsify>::JsType {
        let value = JsValue::from(self.cycles.get());
        value.into()
    }

    pub fn subscribe_cycles(&mut self, callback: CyclesCallback) -> JsValue {
        self.cycles
            .subscribe(callback.unchecked_into())
            .unchecked_into()
    }

    #[must_use]
    pub fn registers(&self) -> <Registers as Tsify>::JsType {
        let value = JsValue::from(self.registers.get());
        value.into()
    }

    pub fn subscribe_registers(&mut self, callback: RegistersCallback) -> Unsubscribe {
        self.registers
            .subscribe(callback.unchecked_into())
            .unchecked_into()
    }

    pub fn memory(&mut self, address: u32) -> <Cell as Tsify>::JsType {
        let value = JsValue::from(self.memory.get_js_value(address));
        value.into()
    }

    pub fn subscribe_memory(&mut self, address: u32, callback: MemoryCallback) -> Unsubscribe {
        self.memory
            .subscribe(address, callback.unchecked_into())
            .unchecked_into()
    }
}

struct MemoryObserver {
    index: usize,
    snapshot: Memory,
    js_values: HashMap<Address, (z33_emulator::runtime::Cell, <Cell as Tsify>::JsType)>,
    subscribers: Rc<RefCell<HashMap<Address, HashMap<usize, js_sys::Function>>>>,
}

impl MemoryObserver {
    fn new(memory: Memory) -> Self {
        let subscribers = Rc::new(RefCell::new(HashMap::new()));
        MemoryObserver {
            index: 0,
            snapshot: memory,
            js_values: HashMap::new(),
            subscribers,
        }
    }

    fn get_js_value(&mut self, address: Address) -> &<Cell as Tsify>::JsType {
        &self
            .js_values
            .entry(address)
            .or_insert_with(|| {
                let cell = self.snapshot.get(address).unwrap_throw();
                let js_value = Cell::from_runtime_cell(cell).into_js().unwrap_throw();
                (cell.clone(), js_value)
            })
            .1
    }

    fn set(&mut self, memory: Memory) {
        for (address, address_subscribers) in &*self.subscribers.borrow() {
            let old_cell = self.snapshot.get(*address).unwrap_throw();
            let new_cell = memory.get(*address).unwrap_throw();
            // Only notify if the cell has changed
            if old_cell == new_cell {
                continue;
            }

            let cell = Cell::from_runtime_cell(new_cell).into_js().unwrap_throw();
            for callback in address_subscribers.values() {
                callback.call1(&JsValue::NULL, &cell).unwrap_throw();
            }
        }

        // Clean up modified js_values
        self.js_values
            .retain(|address, (cell, _)| cell == memory.get(*address).unwrap_throw());

        self.snapshot = memory;
    }

    fn subscribe(&mut self, address: Address, callback: js_sys::Function) -> JsValue {
        let index = self.index.wrapping_add(1);
        self.index = index;
        let mut subscribers = self.subscribers.borrow_mut();
        let address_subscribers = subscribers.entry(address).or_default();
        address_subscribers.insert(index, callback);
        let weak_ref = Rc::downgrade(&self.subscribers);

        Closure::once_into_js(move || {
            let Some(subscribers) = weak_ref.upgrade() else {
                return;
            };
            let mut subscribers = subscribers.borrow_mut();
            // Remove the callback from the address subscribers
            if let Some(address_subscribers) = subscribers.get_mut(&address) {
                address_subscribers.remove(&index);

                // If there are no more subscribers for the address, remove the address
                if address_subscribers.is_empty() {
                    subscribers.remove(&address);
                }
            }
        })
    }
}

struct Observable<T: Tsify> {
    index: usize,
    subscribers: Rc<RefCell<HashMap<usize, js_sys::Function>>>,
    value: T,
    js_value: T::JsType,
}

impl<T: Tsify> Observable<T> {
    fn new(value: T) -> Self
    where
        T: Serialize + Clone,
    {
        let subscribers = Rc::new(RefCell::new(HashMap::new()));
        let js_value = value.into_js().unwrap_throw();
        Observable {
            index: 0,
            subscribers,
            value,
            js_value,
        }
    }

    fn subscribe(&mut self, callback: js_sys::Function) -> JsValue {
        let index = self.index.wrapping_add(1);
        self.index = index;
        self.subscribers.borrow_mut().insert(index, callback);
        let weak_ref = Rc::downgrade(&self.subscribers);

        Closure::once_into_js(move || {
            let Some(subscribers) = weak_ref.upgrade() else {
                return;
            };
            subscribers.borrow_mut().remove(&index);
        })
    }

    fn set(&mut self, value: T)
    where
        T: Serialize + PartialEq,
        T::JsType: std::ops::Deref<Target = JsValue>,
    {
        if self.value == value {
            return;
        }

        self.value = value;
        self.js_value = self.value.into_js().unwrap_throw();
        for callback in self.subscribers.borrow().values() {
            callback
                .call1(&JsValue::NULL, &self.js_value)
                .unwrap_throw();
        }
    }

    fn get(&self) -> &T::JsType {
        &self.js_value
    }
}
