//! The parsing module.
//! Here all the code for parsing stuff is located.
//!
//! ``parse_file`` parses a file, and is the most
//! common public interface into the module.
mod lexer;
mod expression;

use crate::prelude::*;
use expression::prelude::*;
use lexer::{
    Error as LexingError, Lexer, LexingResult, Token,
    TokenKind,
};
use std::io::Error as IoError;

#[derive(Debug)]
pub enum CodeUnit {
    /// A constant value, that is evaluated
    /// at compile time. Most things are this type of
    /// value, even type aliases and functions.
    Constant {
        namespace: Id,
        pos: Pos,
        name: TinyString,
        value: Expression,
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
}

impl Parser<'_> {
    /// Makes sure that the next value is the
    /// correct kind. If not, it will create an
    /// error.
    fn kind(
        &mut self,
        kind: TokenKind,
        pattern: &'static str,
    ) -> ParsingResult<Pos> {
        let token = self.lexer.next_token()?;
        if token.kind == kind {
            Ok(token.pos)
        } else {
            Err(Error::InvalidToken {
                pos: token.pos,
                got: token.kind,
                pattern,
            })
        }
    }

    /// Returns ``Ok(Some(pos))`` and moves to
    /// the next token if the next token has
    /// the given kind. Otherwise, it doesn't do
    /// anything and returns ``Ok(None)``.
    ///
    /// The only errors are when the Lexer fails
    /// in some way. The only fail that isn't counted
    /// is the ``LexingError::EndOfFile``, that returns
    /// ``Ok(None)`` too.
    fn try_kind(
        &mut self,
        kind: TokenKind,
    ) -> LexingResult<Option<Pos>> {
        let token = match self.lexer.peek_token(0) {
            Ok(token) => token,
            Err(LexingError::EndOfFile) => return Ok(None),
            Err(err) => return Err(err),
        };

        if token.kind == kind {
            Ok(Some(token.pos))
        } else {
            Ok(None)
        }
    }

    fn peek_token(
        &mut self,
        n_tokens_forward: usize,
    ) -> LexingResult<Token> {
        self.lexer.peek_token(n_tokens_forward)
    }

    fn next_token(&mut self) -> LexingResult<Token> {
        self.lexer.next_token()
    }
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

    let mut parser = Parser {
        lexer,
        namespace_id_builder: namespace_ids,
    };

    parse_namespace(&mut parser, namespace_id, false)?;

    Ok(())
}

fn parse_namespace(
    parser: &mut Parser,
    namespace_id: u32,
    inside_brackets: bool,
) -> ParsingResult<()> {
    const PATTERN: &str = "mod { ... namespace items ... }";
    if inside_brackets {
        parser.kind(TokenKind::Keyword("mod"), PATTERN);
        parser.kind(TokenKind::Bracket('{'), PATTERN);
    }

    loop {
        if inside_brackets
            && parser
                .try_kind(TokenKind::ClosingBracket('{'))?
                .is_some()
        {
            break;
        }

        match parse_code_unit(parser, namespace_id) {
            Ok(unit) => {
                println!("{:#?}", unit);
            }
            Err(Error::Lexer(LexingError::EndOfFile)) => {
                break
            }
            Err(err) => return Err(err),
        }
    }

    if inside_brackets {
        parser
            .kind(TokenKind::ClosingBracket('{'), PATTERN);
    }

    Ok(())
}

fn parse_code_unit(
    parser: &mut Parser,
    namespace_id: u32,
) -> ParsingResult<String> {

    Ok(format!("Hello world!"))
}

pub struct Identifier {
    pub pos: Pos,
    pub name: TinyString,
}

fn parse_identifier(
    parser: &mut Parser,
    pattern: &'static str,
) -> ParsingResult<Identifier> {
    match parser.next_token()? {
        Token {
            kind: TokenKind::Identifier(name),
            pos,
        } => Ok(Identifier {
            pos,
            name,
        }),
        Token {
            kind,
            pos,
        } => Err(Error::InvalidToken {
            pos,
            got: kind,
            pattern,
        }),
    }
}

type ParsingResult<T> = Result<T, Error>;

const CONST_ASSIGN_PATTERN: &str =
    "``[name] :: [constant expression];``";
const DECLARATION_PATTERN: &str =
    "``[name] : (optional)[type] = [expression];``";
const ASSIGN_PATTERN: &str =
    "``[expression] [assign_operator] [expression];``";

#[derive(Debug)]
pub enum Error {
    Lexer(LexingError),
    Io(IoError),
    InvalidToken {
        pos: Pos,
        got: TokenKind,
        /// The whole thing that was trying to be parsed.
        pattern: &'static str,
    },
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
