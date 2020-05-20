//! The parsing module.
//! Here all the code for parsing stuff is located.
//!
//! ``parse_file`` parses a file, and is the most
//! common public interface into the module.
mod lexer;

use crate::prelude::*;
use std::io::{ Error as IoError };
use lexer::{ 
    Error as LexingError, 
    Token, 
    TokenKind,
    Lexer,
};

#[derive(Debug)]
pub enum CodeUnit {
    /// A constant value, that is evaluated
    /// at compile time. Most things are this type of
    /// value, even type aliases and functions.
    Constant {
        namespace: Id,
        pos: Pos,
        name: TinyString,
        // value: Expression,
    },
    /// A type that isn't considered
    /// the same type as the type it wraps.
    UniqueType {
        namespace: Id,
        pos: Pos,
        name: TinyString,
        // value: Expression,
    },
    /// Another file that needs to be parsed.
    File {
        caller_pos: Option<Pos>,
        container_namespace: Id,
        namespace: Id,
        path: String,
        file: TinyString,
    },
}

#[derive(Debug)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    namespace_id_builder: &'a IdBuilder,
    namespace_id: Id,
}

pub fn parse_file(
    path: impl AsRef<std::path::Path>,
    file: TinyString,
    namespace_ids: &IdBuilder,
    namespace_id: Id,
    code_unit_callback: impl FnMut(CodeUnit),
) -> ParsingResult<()> {
    let source = std::fs::read_to_string(path)?;
    let mut lexer = Lexer::new(file, &source);

    parse_namespace(&mut lexer, namespace_id, false)?;

    Ok(())
}

fn parse_namespace(
    lexer: &mut Lexer,
    namespace_id: Id,
    inside_brackets: bool,
) -> ParsingResult<()> {
    if inside_brackets {
        todo!("Parse kind");
    }

    todo!("Parse a namespace");
}

// fn parse_kind(
//     lexer: &mut Lexer,
//     namespace_id: Id
// )

type ParsingResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Lexer(LexingError),
    Io(IoError),
}

impl From<LexingError> for Error {
    fn from(err: LexingError) -> Error {
        Error::Lexer(err)
    }
}

impl From<IoError> for Error {
    fn from(err: IoError) -> Error {
        Error::Io(err)
    }
}
