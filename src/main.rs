#![allow(dead_code)]
#![allow(unused)]
#![feature(test)]

#[macro_use]
extern crate lazy_static;

mod tree_structure;
mod pos;
mod id;
mod tiny_string;
mod parser;

/// Common imports that most things need.
/// This is to make the imports at the top
/// of modules less cluttered.
mod prelude {
    pub use crate::tiny_string::TinyString;
    pub use crate::id::{ Id, IdBuilder };
    pub use crate::pos::Pos;
}

fn main() {
    let ids = id::IdBuilder::new();
    parser::parse_file(
        "testing.wod",
        "testing".into(),
        &ids,
        ids.create_id(),
        |unit| println!("{:#?}", unit),
    ).unwrap();
}
