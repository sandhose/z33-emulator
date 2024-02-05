#![forbid(unsafe_code)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::*;

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
    pub fn new(files: InputFiles) -> Self {
        let fs = InMemoryFilesystem::new(files.0);
        let preprocessor = Preprocessor::new(fs);
        Self { preprocessor }
    }

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
    #[wasm_bindgen]
    pub fn parse(source: String) -> Result<Program, JsValue> {
        let program = z33_emulator::parser::parse_new::<z33_emulator::parser::Error<_>>(&source);
        match program {
            Ok(program) => Ok(Self { source, program }),
            Err(e) => Err(format!("{e:#?}").into()),
        }
    }

    #[wasm_bindgen(getter)]
    pub fn source(&self) -> String {
        self.source.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn ast(&self) -> String {
        let ast = self.program.to_node();
        let ast = ast.map_location(&AbsoluteLocation::default());
        format!("{ast}")
    }

    #[wasm_bindgen(getter)]
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

    #[wasm_bindgen]
    pub fn compile(&self, entrypoint: &str) -> Result<Computer, JsValue> {
        let (computer, debug_info) =
            compile(self.program.inner.clone(), entrypoint).map_err(|e| format!("{e}"))?;

        Ok(Computer {
            debug_info,
            computer,
        })
    }
}

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Labels(Vec<String>);

#[wasm_bindgen]
pub struct Computer {
    debug_info: z33_emulator::compiler::DebugInfo,
    computer: z33_emulator::runtime::Computer,
}

#[derive(Serialize, Tsify)]
#[serde(tag = "type", rename_all = "lowercase")]
#[tsify(into_wasm_abi)]
pub enum Cell {
    Instruction { instruction: String },
    Word { word: i64 },
    Empty,
}

impl Cell {
    pub fn from_runtime_cell(cell: &z33_emulator::runtime::Cell) -> Self {
        match cell {
            z33_emulator::runtime::Cell::Instruction(s) => Self::Instruction {
                instruction: s.to_string(),
            },
            z33_emulator::runtime::Cell::Word(w) => Self::Word { word: *w },
            z33_emulator::runtime::Cell::Empty => Self::Empty,
        }
    }
}

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Registers {
    a: Cell,
    b: Cell,
    pc: u32,
    sp: u32,
    sr: i64,
}

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct MemoryView {
    start: u32,
    end: u32,
    cells: Vec<Cell>,
}

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct LabelsWithAddresses(BTreeMap<String, u32>);

#[wasm_bindgen]
impl Computer {
    pub fn step(&mut self) -> Result<bool, JsValue> {
        match self.computer.step() {
            Ok(()) => Ok(false),
            Err(ProcessorError::Reset) => Ok(true),
            Err(e) => Err(format!("{e}").into()),
        }
    }

    #[wasm_bindgen(getter)]
    pub fn labels(&self) -> LabelsWithAddresses {
        LabelsWithAddresses(self.debug_info.labels.clone())
    }

    #[wasm_bindgen(getter)]
    pub fn registers(&self) -> Registers {
        let registers = &self.computer.registers;
        Registers {
            a: Cell::from_runtime_cell(&registers.a),
            b: Cell::from_runtime_cell(&registers.b),
            pc: registers.pc,
            sp: registers.sp,
            sr: registers.sr.bits(),
        }
    }

    pub fn memory_view(&self, start: u32, end: u32) -> Result<MemoryView, JsValue> {
        let mut cells = Vec::new();
        for i in start..end {
            let cell = self.computer.memory.get(i).map_err(|e| format!("{e}"))?;
            cells.push(Cell::from_runtime_cell(&cell));
        }
        Ok(MemoryView { start, end, cells })
    }
}
