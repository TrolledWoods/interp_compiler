#![allow(dead_code)]
#![allow(unused)]

#[macro_use]
extern crate lazy_static;

mod pos;
mod id;
mod tiny_string;
mod parser;

mod prelude {
    pub use crate::tiny_string::TinyString;
    pub use crate::id::{ Id, IdBuilder };
    pub use crate::pos::Pos;
}

fn main() {
}
