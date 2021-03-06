use std::collections::HashMap;

use serde::Serialize;
use wasm_bindgen::prelude::*;

use z33_emulator::{
    compiler::layout,
    constants as C,
    parser::location::{AbsoluteLocation, Lines, RelativeLocation},
    preprocessor::{preprocess, InMemoryFilesystem},
};

#[derive(Default, Serialize)]
struct Output {
    ast: Option<String>,
    preprocessed: Option<String>,
    memory: Vec<(C::Address, String)>,
    labels: HashMap<String, C::Address>,
    error: Option<String>,
}

#[wasm_bindgen]
pub fn dump(source: &str) -> Result<JsValue, JsValue> {
    let mut output = Output::default();
    let mut files = HashMap::new();
    files.insert("-".into(), source.to_string());

    let fs = InMemoryFilesystem::new(files);
    let preprocessed = preprocess(&fs, &"-".into());

    if let Err(e) = preprocessed {
        output.error = Some(format!("{}", e));
        return Ok(serde_wasm_bindgen::to_value(&output)?);
    }

    let source = &preprocessed.unwrap();

    output.preprocessed = Some(source.clone());

    let program = z33_emulator::parser::parse_new::<z33_emulator::parser::Error<_>>(source); // TODO: the error is tied to the input

    if let Err(e) = program {
        output.error = Some(format!("{:#?}", e));
        return Ok(serde_wasm_bindgen::to_value(&output)?);
    }

    let program = program.unwrap();

    let ast = program.to_node();

    // Transform the AST relative locations to absolute ones
    let ast = ast.transform_location(
        &AbsoluteLocation::default(),
        &RelativeLocation::into_absolute,
    );

    // Map the AST absolute offsets to line/col locations
    let lines = Lines::new(source);
    let ast = ast.map_location(&|l| l.to_line_aware(&lines));

    output.ast = Some(format!("{}", ast));

    let layout = layout(program.inner);

    if let Err(e) = layout {
        output.error = Some(format!("{}", e));
        return Ok(serde_wasm_bindgen::to_value(&output)?);
    }

    let layout = layout.unwrap();
    output.memory = layout.memory_report();
    output.labels = layout.labels;

    Ok(serde_wasm_bindgen::to_value(&output)?)
}
