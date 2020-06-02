#![allow(dead_code)]
#![allow(unused)]
#![feature(box_syntax)]
#![feature(box_patterns)]

#[macro_use]
extern crate lazy_static;

mod id;
mod operators;
mod parser;
mod pos;
mod tiny_string;
mod tree_structure;
mod types;
mod interp;

/// Common imports that most things need.
/// This is to make the imports at the top
/// of modules less cluttered.
mod prelude {
    pub use crate::id::{Id, IdBuilder};
    pub use crate::pos::Pos;
    pub use crate::tiny_string::TinyString;
    pub use crate::types::{PrimitiveKind, Type, TypeIds};
}

fn main() {
    let ids = id::IdBuilder::new();
    parser::parse_file(
        "testing.wod",
        "testing".into(),
        &ids,
        ids.create_id(),
        |unit| println!("{:#?}", unit),
    )
    .unwrap();
}
