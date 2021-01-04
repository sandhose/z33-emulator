use wasm_bindgen::prelude::*;

use z33_emulator::{
    parse,
    parser::location::{AbsoluteLocation, Lines, RelativeLocation},
};

#[wasm_bindgen]
pub fn dump(source: &str) -> String {
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

    format!("{}", ast)
}
