#![forbid(unsafe_code)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::*;

use z33_emulator::{
    compiler::layout,
    parser::location::{AbsoluteLocation, MapLocation, RelativeLocation},
    preprocessor::{InMemoryFilesystem, Preprocessor},
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
    program: z33_emulator::parser::location::Located<
        z33_emulator::parser::Program<RelativeLocation>,
        RelativeLocation,
    >,
}

#[wasm_bindgen]
impl Program {
    #[wasm_bindgen]
    pub fn parse(source: &str) -> Result<Program, JsValue> {
        let program = z33_emulator::parser::parse_new::<z33_emulator::parser::Error<_>>(source);
        match program {
            Ok(p) => Ok(Self { program: p }),
            Err(e) => Err(format!("{e:#?}").into()),
        }
    }

    #[wasm_bindgen(getter)]
    pub fn ast(&self) -> String {
        let ast = self.program.to_node();
        let ast = ast.map_location(&AbsoluteLocation::default());
        format!("{ast}")
    }

    pub fn layout(self) -> Result<Layout, JsValue> {
        let layout = layout(self.program.inner).map_err(|e| format!("{e}"))?;
        Ok(Layout { layout })
    }
}

#[wasm_bindgen]
pub struct Layout {
    layout: z33_emulator::compiler::layout::Layout<RelativeLocation>,
}

#[wasm_bindgen]
impl Layout {
    #[wasm_bindgen(getter)]
    pub fn memory(&self) -> MemoryReport {
        MemoryReport(self.layout.memory_report())
    }

    #[wasm_bindgen(getter)]
    pub fn labels(&self) -> Labels {
        Labels(self.layout.labels.clone())
    }
}

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct MemoryReport(BTreeMap<u32, String>);

#[derive(Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Labels(BTreeMap<String, u32>);
