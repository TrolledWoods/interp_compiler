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
        pos: Pos,
        namespace: Id,
        name: TinyString,
        const_args: Vec<(TinyString, Expression)>,
        type_expression: Option<Expression>,
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
    ) -> LexingResult<bool> {
        let token = match self.lexer.peek_token(0) {
            Ok(token) => token,
            Err(LexingError::EndOfFile) => return Ok(false),
            Err(err) => return Err(err),
        };

        if token.kind == kind {
            self.lexer.next_token().unwrap();
            Ok(true)
        } else {
            Ok(false)
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
    namespace_id: Id,
    inside_brackets: bool,
) -> ParsingResult<()> {
    const PATTERN: &str = "mod { ... namespace items ... }";
    if inside_brackets {
        parser.kind(TokenKind::Keyword("mod"), PATTERN);
        parser.kind(TokenKind::Bracket('{'), PATTERN);
    }

    loop {
        if parser.lexer.at_end_of_file() {
            break;
        }

        if inside_brackets
            && parser
                .try_kind(TokenKind::ClosingBracket('{'))?
        {
            break;
        }

        match parse_code_unit(parser, namespace_id) {
            Ok(unit) => {
                println!("{:#?}", unit);
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
    namespace_id: Id,
) -> ParsingResult<CodeUnit> {
    const CODE_UNIT_PATTERN: &str =
         "<name>[a: <const_arg>] : <optional type> : <value>;";

    // All of these are basically just constants
    let name = parse_identifier(parser, CODE_UNIT_PATTERN)?;

    // Parse constant arguments
    if parser.try_kind(TokenKind::Bracket('['))? {
        // TODO: List parsing function.
        parser.kind(
            TokenKind::ClosingBracket(']'), 
            CODE_UNIT_PATTERN,
        );
    }

    // Parse the ``: <optional type> :`` part.
    parser.kind(
        TokenKind::Special(":"),
        CODE_UNIT_PATTERN,
    )?;

    // If we don't have another ':' immediately after,
    // then we know we have a type, and then another ':'.
    let type_expr = if !parser.try_kind(TokenKind::Special(":"))? {
        let expression = parse_expression(parser, namespace_id)?;

        parser.kind(
            TokenKind::Special(":"),
            CODE_UNIT_PATTERN,
        )?;

        Some(expression)
    } else {
        None
    };

    dbg!(parser.peek_token(0)?);
    // Parse the actual value.
    let value = parse_expression(parser, namespace_id)?;

    parser.kind(
        TokenKind::Special(";"),
        CODE_UNIT_PATTERN,
    )?;

    Ok(CodeUnit::Constant {
        pos: name.pos,
        namespace: namespace_id,
        name: name.name,
        const_args: vec![],
        type_expression: type_expr,
        value,
    })
}

fn parse_expression(
    parser: &mut Parser,
    namespace_id: Id,
) -> ParsingResult<Expression> {
    Ok(parse_expression_req(parser, namespace_id, 0)?)
}

fn parse_expression_req(
    parser: &mut Parser,
    namespace_id: Id,
    priority: u8,
) -> ParsingResult<Expression> {
    let value = parse_value(parser, namespace_id)?;
    Ok(value)
}

/// Parses function calls, constant calls, identifiers,
/// unary operators, e.t.c.
fn parse_value(
    parser: &mut Parser,
    namespace_id: Id,
) -> ParsingResult<Expression> {
    let (pos, kind) = parser.peek_token(0)?.into_parts();

    let value = match kind {
        TokenKind::Bracket('(') => {
            // Block!
            todo!("Parsing blocks");
        }
        TokenKind::Identifier(name) => {
            parser.next_token()?;

            Expression {
                pos: Some(pos), 
                kind: Node::Identifier(namespace_id, name),
            }
        }
        _ => todo!("Error message for invalid value token"),
    };

    // TODO: Check for brackets here indicating function calls.

    Ok(value)
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
