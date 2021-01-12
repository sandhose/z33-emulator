use std::collections::HashMap;

use wasm_bindgen::prelude::*;

use z33_emulator::{
    parse,
    parser::location::{AbsoluteLocation, Lines, RelativeLocation},
    preprocessor::{preprocess, InMemoryFilesystem},
};

#[wasm_bindgen]
pub fn dump(source: &str) -> String {
    let mut files = HashMap::new();
    files.insert("-".into(), source.to_string());

    let fs = InMemoryFilesystem::new(files);
    let preprocessed = preprocess(&fs, &"-".into());

    if let Err(e) = preprocessed {
        return format!("{}", e);
    }

    let source = &preprocessed.unwrap();

    let program = parse(source); // TODO: the error is tied to the input

    if let Err(e) = program {
        return format!("{}", e);
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

    format!("{}\n\n{}", ast, source)
}
