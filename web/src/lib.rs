#![allow(
    /* Tsify derive generates this */
    clippy::empty_docs
)]
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::rc::Rc;

use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::*;

use z33_emulator::constants::Address;
use z33_emulator::runtime::Memory;
use z33_emulator::{
    compiler::compile,
    parser::location::{AbsoluteLocation, MapLocation, RelativeLocation},
    preprocessor::{InMemoryFilesystem, Preprocessor},
    runtime::ProcessorError,
};

#[wasm_bindgen(start)]
fn start() {
    console_error_panic_hook::set_once();
    tracing_wasm::set_as_global_default();
}

#[wasm_bindgen]
pub struct InMemoryPreprocessor {
    preprocessor: Preprocessor<InMemoryFilesystem>,
}

#[derive(Default, Deserialize, Tsify)]
#[tsify(from_wasm_abi)]
pub struct InputFiles(HashMap<PathBuf, String>);

#[wasm_bindgen]
impl InMemoryPreprocessor {
    #[wasm_bindgen(constructor)]
    #[must_use]
    pub fn new(files: InputFiles) -> Self {
        let fs = InMemoryFilesystem::new(files.0);
        let preprocessor = Preprocessor::new(fs);
        Self { preprocessor }
    }

    /// Preprocess the file at the given path
    ///
    /// # Errors
    ///
    /// Returns an error if the file could not be preprocessed.
    pub fn preprocess(&mut self, path: &str) -> Result<String, JsValue> {
        let path = PathBuf::from(path);
        self.preprocessor.load(&path);
        let source = self.preprocessor.preprocess(&path);
        match source {
            Ok(s) => Ok(s),
            Err(e) => Err(format!("{e}").into()),
        }
    }
}

#[wasm_bindgen]
pub struct Program {
    source: String,
    program: z33_emulator::parser::location::Located<
        z33_emulator::parser::Program<RelativeLocation>,
        RelativeLocation,
    >,
}

#[wasm_bindgen]
impl Program {
    /// Parse a program from a string
    ///
    /// # Errors
    ///
    /// Returns an error if the program could not be parsed.
    #[wasm_bindgen]
    pub fn parse(source: String) -> Result<Program, JsValue> {
        let program = z33_emulator::parser::parse_new::<z33_emulator::parser::Error<_>>(&source);
        match program {
            Ok(program) => Ok(Self { source, program }),
            Err(e) => Err(format!("{e:#?}").into()),
        }
    }

    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn source(&self) -> String {
        self.source.clone()
    }

    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn ast(&self) -> String {
        let ast = self.program.to_node();
        let ast = ast.map_location(&AbsoluteLocation::default());
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

    /// Compile the program at the given entrypoint
    ///
    /// # Errors
    ///
    /// Returns an error if the program could not be compiled.
    #[wasm_bindgen]
    pub fn compile(&self, entrypoint: &str) -> Result<Computer, JsValue> {
        tracing::info!("Compiling");
        let (computer, debug_info) =
            compile(self.program.inner.clone(), entrypoint).map_err(|e| format!("{e}"))?;

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

        tracing::info!("Compiled");

        Ok(Computer {
            debug_info,
            computer,
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

#[wasm_bindgen]
pub struct Computer {
    debug_info: z33_emulator::compiler::DebugInfo,
    computer: z33_emulator::runtime::Computer,
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
        let res = match self.computer.step() {
            Ok(()) => Ok(false),
            Err(ProcessorError::Reset) => Ok(true),
            Err(e) => Err(format!("{e}").into()),
        };

        let registers = Registers {
            a: Cell::from_runtime_cell(&self.computer.registers.a),
            b: Cell::from_runtime_cell(&self.computer.registers.b),
            pc: self.computer.registers.pc,
            sp: self.computer.registers.sp,
            sr: self.computer.registers.sr.bits(),
        };
        self.registers.set(registers);
        self.cycles.set(Cycles(self.computer.cycles));
        self.memory.set(self.computer.memory.clone());

        res
    }

    #[wasm_bindgen(getter)]
    #[must_use]
    pub fn labels(&self) -> LabelsWithAddresses {
        LabelsWithAddresses(self.debug_info.labels.clone())
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
