//! The parsing module.
//! Here all the code for parsing stuff is located.
//!
//! ``parse_file`` parses a file, and is the most
//! common public interface into the module.
mod expression;
mod lexer;

use crate::prelude::*;
use expression::prelude::*;
use lexer::{
    Error as LexingError, Lexer, LexingResult, Token,
    TokenKind,
};
use std::io::Error as IoError;
use std::collections::BTreeMap;

#[derive(Debug)]
pub enum CodeUnit {
    /// A constant value, that is evaluated
    /// at compile time. Most things are this type of
    /// value, even type aliases and functions.
    Constant {
        pos: Pos,
        namespace: Id,
        name: TinyString,
        const_args: BTreeMap<TinyString, (Pos, Expression)>,
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
                expected: vec![],
            })
        }
    }

    fn try_kind(
        &mut self,
        kind: TokenKind,
    ) -> LexingResult<bool> {
        let token = match self.lexer.peek_token(0) {
            Ok(token) => token,
            Err(LexingError::EndOfFile) => {
                return Ok(false)
            }
            Err(err) => return Err(err),
        };

        if token.kind == kind {
            self.lexer.next_token().unwrap();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn maybe_kind(
        &mut self,
        kind: TokenKind,
    ) -> LexingResult<Option<Pos>> {
        let token = match self.lexer.peek_token(0) {
            Ok(token) => token,
            Err(LexingError::EndOfFile) => {
                return Ok(None)
            }
            Err(err) => return Err(err),
        };

        if token.kind == kind {
            let Token {
                pos, ..
            } = self.lexer.next_token().unwrap();
            Ok(Some(pos))
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

        match parse_constant(parser, namespace_id) {
            Ok(unit) => {
                println!("{:?}", unit);
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

fn parse_constant(
    parser: &mut Parser,
    namespace_id: Id,
) -> ParsingResult<CodeUnit> {
    const PATTERN: &str =
         "<name>[a: <const_arg>] : <optional type> : <value>;";

    // All of these are basically just constants
    let name = parse_identifier(parser, PATTERN)?;

    let mut const_args = BTreeMap::new();
    if parser.peek_token(0)?.kind == TokenKind::Bracket('[') {
        parse_named_list(
            parser,
            '[',
            |parser| parse_expression(parser, namespace_id),
            |name, elem| {
                match const_args.insert(
                    name.name, 
                    (name.pos, elem),
                ) {
                    Some((old_pos, _)) => Err(
                        Error::DuplicateConstArg {
                            pos: name.pos,
                            old_pos,
                            name: name.name,
                        }
                    ),
                    _ => Ok(()),
                }
            }
        )?;
    }

    // Parse the ``: <optional type> :`` part.
    parser.kind(TokenKind::Special(":"), PATTERN)?;

    // If we don't have another ':' immediately after,
    // then we know we have a type, and then another ':'.
    let type_expr = if !parser
        .try_kind(TokenKind::Special(":"))?
    {
        let expression =
            parse_expression(parser, namespace_id)?;

        parser.kind(TokenKind::Special(":"), PATTERN)?;

        Some(expression)
    } else {
        None
    };

    // Parse the actual value.
    let value = parse_expression(parser, namespace_id)?;

    parser.kind(TokenKind::Special(";"), PATTERN)?;

    Ok(CodeUnit::Constant {
        pos: name.pos,
        namespace: namespace_id,
        name: name.name,
        const_args,
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

    let mut value = match kind {
        TokenKind::IntLiteral(num) => {
            parser.next_token()?;
            Expression {
                pos: Some(pos),
                kind: Node::IntLiteral(num),
            }
        }
        TokenKind::FloatLiteral(num) => {
            parser.next_token()?;
            Expression {
                pos: Some(pos),
                kind: Node::FloatLiteral(num),
            }
        }
        TokenKind::Operator(op) => {
            let Token { pos, .. } = parser.next_token()?;
            let inside = parse_value(parser, namespace_id)?;

            Expression {
                pos: Some(pos),
                kind: 
                    Node::UnaryOperator(op, Box::new(inside)),
            }
        }
        TokenKind::Bracket('(') => {
            // Block!
            todo!("Parsing blocks");
        }
        TokenKind::Bracket('{') => {
            // Collection!
            let mut list = BTreeMap::new();
            let pos = parse_named_list(
                parser,
                '{',
                |parser| 
                    parse_expression(parser, namespace_id),
                |ident, elem| {
                    if let Some(
                        (old_pos, _old_elem),
                    ) = list.insert(
                        ident.name, 
                        (ident.pos, elem),
                    ) {
                        Err(Error::DuplicateCollectionMembers{
                            pos: ident.pos,
                            old_pos,
                            name: ident.name,
                        })
                    } else { Ok(()) }
                },
            )?;

            Expression {
                pos: Some(pos),
                kind: Node::Collection(list),
            }
        }
        TokenKind::Primitive(kind) => {
            parser.next_token()?;
            Expression {
                pos: Some(pos),
                kind: Node::Primitive(kind),
            }
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

    // value is an option because we need to move it to
    // a vector temporarily
    let mut value = Some(value);
    while parser.peek_token(0)?.kind == 
            TokenKind::Bracket('(') {
        let mut args = vec![value.take().unwrap()];
        let pos = parse_list(
            parser,
            '(',
            |parser| parse_expression(parser, namespace_id),
            |elem| {
                args.push(elem);
                Ok(())
            },
        )?;

        value = Some(Expression {
            pos: Some(pos),
            kind: Node::FunctionCall(args),
        });
    }

    Ok(value.unwrap())
}

fn parse_list<'a, Elem>(
    parser: &mut Parser<'a>,
    bracket_kind: char,
    mut parse_element: impl FnMut(
        &mut Parser<'a>
    ) -> ParsingResult<Elem>,
    mut on_get_element: 
        impl FnMut(Elem) -> ParsingResult<()>,
) -> ParsingResult<Pos> {
    let start = parser.kind(
        TokenKind::Bracket(bracket_kind),
        "Wanted bracket",
    )?;

    let end = loop {
        if let Some(end) = parser.maybe_kind(
            TokenKind::ClosingBracket(bracket_kind),
        )? {
            break end;
        }

        let element = parse_element(parser)?;
        on_get_element(element);

        match parser.next_token()? {
            Token { 
                kind: TokenKind::Special(","),
                .. 
            } => (),
            Token {
                kind: TokenKind::ClosingBracket(bracket_kind),
                pos,
            } => break pos,
            Token {
                pos,
                kind,
            } => return Err(Error::InvalidToken {
                pos,
                got: kind,
                pattern: "{ [name]: [item], [name]: [item] }",
                expected: vec![",", "}"],
            }),
        }
    };

    Ok(start.join(end))
}

fn parse_named_list<'a, Elem>(
    parser: &mut Parser<'a>,
    bracket_kind: char,
    mut parse_element: impl FnMut(
        &mut Parser<'a>
    ) -> ParsingResult<Elem>,
    mut on_get_element: 
        impl FnMut(Identifier, Elem) -> ParsingResult<()>,
) -> ParsingResult<Pos> {
    let start = parser.kind(
        TokenKind::Bracket(bracket_kind),
        "Wanted bracket",
    )?;

    let mut auto_name_counter = 0;
    let end = loop {
        if let Some(end) = parser.maybe_kind(
            TokenKind::ClosingBracket(bracket_kind),
        )? {
            break end;
        }

        let name = parse_identifier(
            parser,
            "{ [name]: [item], [name]: [item] }",
        )?;

        parser.kind(
            TokenKind::Special(":"),
            "{ [name]: [item] }",
        )?;

        let element = parse_element(parser)?;
        on_get_element(name, element)?;

        match parser.next_token()? {
            Token { 
                kind: TokenKind::Special(","),
                .. 
            } => (),
            Token {
                kind: TokenKind::ClosingBracket(bracket_kind),
                pos,
            } => break pos,
            Token {
                pos,
                kind,
            } => return Err(Error::InvalidToken {
                pos,
                got: kind,
                pattern: "{ [name]: [item], [name]: [item] }",
                expected: vec![",", "}"],
            }),
        }
    };

    Ok(start.join(end))
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
        } => Ok(Identifier { pos, name }),
        Token { kind, pos } => Err(Error::InvalidToken {
            pos,
            got: kind,
            pattern,
            expected: vec![],
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
        expected: Vec<&'static str>,
    },
    DuplicateConstArg {
        pos: Pos,
        old_pos: Pos,
        name: TinyString,
    },
    DuplicateCollectionMembers {
        pos: Pos,
        old_pos: Pos,
        name: TinyString,
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
