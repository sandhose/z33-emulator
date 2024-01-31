#![forbid(unsafe_code)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
use std::collections::HashMap;
use std::path::PathBuf;

use serde::Serialize;
use tsify::Tsify;
use wasm_bindgen::prelude::*;

use z33_emulator::{
    compiler::layout,
    parser::location::{AbsoluteLocation, MapLocation},
    preprocessor::{InMemoryFilesystem, Preprocessor},
};

#[derive(Default, Serialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct Output {
    ast: Option<String>,
    preprocessed: Option<String>,
    memory: Vec<(u32, String)>,
    labels: HashMap<String, u32>,
    error: Option<String>,
}

#[wasm_bindgen]
pub fn dump(source: &str) -> Result<Output, JsValue> {
    let mut output = Output::default();
    let mut files = HashMap::new();
    let path = PathBuf::from("-");
    files.insert(path.clone(), source.to_string());

    let fs = InMemoryFilesystem::new(files);
    let preprocessor = Preprocessor::new(fs).and_load(&path);

    let source = match preprocessor.preprocess(&path) {
        Ok(s) => s,
        Err(e) => {
            output.error = Some(format!("{e}"));
            return Ok(output);
        }
    };

    output.preprocessed = Some(source.clone());

    let program = z33_emulator::parser::parse_new::<z33_emulator::parser::Error<_>>(&source); // TODO: the error is tied to the input

    let program = match program {
        Ok(p) => p,
        Err(e) => {
            output.error = Some(format!("{e:#?}"));
            return Ok(output);
        }
    };

    let ast = program.to_node();

    // Transform the AST relative locations to absolute ones
    let ast = ast.map_location(&AbsoluteLocation::default());

    output.ast = Some(format!("{ast}"));

    let layout = layout(program.inner);

    if let Err(e) = layout {
        output.error = Some(format!("{e}"));
        return Ok(output);
    }

    let layout = layout.unwrap();
    output.memory = layout.memory_report();
    output.labels = layout.labels;

    Ok(output)
}
