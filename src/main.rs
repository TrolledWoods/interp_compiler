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
	let type_ids = types::TypeIds::new();
    parser::parse_file(
        "testing.wod",
        "testing".into(),
        &ids,
        ids.create_id(),
        |mut unit| {
			use parser::CodeUnit;
			match &mut unit {
				CodeUnit::Constant { value, .. } => {
					let context = interp::Context {
						type_ids: &type_ids,
					};
					let val = interp::interp(
						context,
						value,
						&mut |_, _| Err(
							interp::GetError::UnknownName
						),
					).unwrap();
					println!("{:?}", val);
				},
				_ => (),
			}
			println!("{:#?}", unit);
		},
    )
    .unwrap();
}
