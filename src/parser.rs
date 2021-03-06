//! The parsing module.
//! Here all the code for parsing stuff is located.
//!
//! ``parse_file`` parses a file, and is the most
//! common public interface into the module.
pub mod expression;
mod lexer;

use crate::operators::{get_operator_info, OpDirection};
use crate::prelude::*;
use expression::prelude::*;
use lexer::{
    Error as LexingError, Lexer, LexingResult, Token,
    TokenKind,
};
use std::collections::BTreeMap;
use std::fmt;
use std::io::Error as IoError;

macro_rules! error {
    ($value:expr) => {{
        println!("{}: {}:{}", file!(), line!(), column!());
        $value
    }};
}

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

struct Parser<'a> {
    lexer: Lexer<'a>,
    /// Yes, this is crap, but it won't be called too
    /// often, so it should be fine.
    add_code_unit: &'a dyn Fn(CodeUnit),
    namespace_id_builder: &'a IdBuilder,
}

impl fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Parser")
            .field("lexer", &self.lexer)
            .field(
                "namespace_id_builder",
                &self.namespace_id_builder,
            )
            .finish()
    }
}

impl Parser<'_> {
    fn create_sub_namespace(&mut self, source: Id) -> Id {
        // TODO: Actually add some metadata as to
        // how namespaces relate to one another
        self.namespace_id_builder.create_id()
    }

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
            Err(LexingError::EndOfFile) => return Ok(None),
            Err(err) => return Err(err),
        };

        if token.kind == kind {
            let Token { pos, .. } =
                self.lexer.next_token().unwrap();
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
    code_unit_callback: impl Fn(CodeUnit) + Sync,
) -> ParsingResult<()> {
    let source = std::fs::read_to_string(path)?;
    let mut lexer = Lexer::new(file, &source);

    let mut parser = Parser {
        lexer,
        namespace_id_builder: namespace_ids,
        add_code_unit: &code_unit_callback,
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
) -> ParsingResult<()> {
    use TokenKind::*;

    const PATTERN: &str = "<name>[a: <const_arg>] : \
                           <optional type> : <value>;";

    parser.kind(
        Keyword("const"),
        "const needed for constant",
    )?;

    // All of these are basically just constants
    let name = parse_identifier(parser, PATTERN)?;

    let const_args = if parser.peek_token(0)?.kind
        == TokenKind::Bracket('[')
    {
        let (pos, list) = parse_named_or_unnamed_list(
            parser,
            '[',
            |parser| parse_expression(parser, namespace_id),
        )?;

        list.into_named(pos)?
    } else {
        BTreeMap::new()
    };

    // If we don't have another ':' immediately after,
    // then we know we have a type, and then another ':'.
    let type_expr =
        if parser.try_kind(TokenKind::Special(":"))? {
            Some(parse_expression(parser, namespace_id)?)
        } else {
            None
        };

    parser.kind(
        AssignOperator(""),
        "Wanted assignemnt operator",
    )?;

    // Parse the actual value.
    let value = parse_expression(parser, namespace_id)?;

    parser.kind(TokenKind::Special(";"), PATTERN)?;

    (parser.add_code_unit)(CodeUnit::Constant {
        pos: name.pos,
        namespace: namespace_id,
        name: name.name,
        const_args,
        type_expression: type_expr,
        value,
    });

    Ok(())
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
    max_order: u8,
) -> ParsingResult<Expression> {
    use TokenKind::*;

    let mut value = parse_value(parser, namespace_id)?;

    loop {
        match parser.peek_token(0)? {
            Token {
                kind: Operator(op),
                pos,
            } => {
                use OpDirection::*;
                let (order, dir) = get_operator_info(op)
                    .expect(
                        "Operators from the lexer should \
                         be valid",
                    );

                match dir {
                    LeftToRight if order <= max_order => {
                        break
                    }
                    RightToLeft if order < max_order => {
                        break
                    }
                    _ => (),
                }

                parser.next_token()?;

                let new_value = parse_expression_req(
                    parser,
                    namespace_id,
                    order,
                )?;

                value = Expression {
                    pos: Some(pos),
                    kind: Node::BinaryOperator(
                        op,
                        box match dir {
                            LeftToRight => {
                                (value, new_value)
                            }
                            RightToLeft => {
                                (new_value, value)
                            }
                        },
                    ),
                };
            }
            _ => break,
        }
    }

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
                kind: Node::UnaryOperator(
                    op,
                    Box::new(inside),
                ),
            }
        }
        TokenKind::Keyword("map") => {
            parse_map(parser, namespace_id)?
        }
        TokenKind::Bracket('(') => {
            parse_block(parser, namespace_id)?
        }
        TokenKind::Bracket('{') => {
            // Collection!
            let mut list = BTreeMap::new();
            let pos = parse_named_list(
                parser,
                '{',
                |parser| {
                    parse_expression(parser, namespace_id)
                },
                |ident, elem| {
                    if let Some((old_pos, _old_elem)) = list
                        .insert(
                            ident.name,
                            (ident.pos, elem),
                        )
                    {
                        Err(Error::DuplicateListItems {
                            pos: ident.pos,
                            old_pos,
                            name: ident.name,
                        })
                    } else {
                        Ok(())
                    }
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

            let mut args = vec![];
            if parser.peek_token(0)?.kind
                == TokenKind::Bracket('[')
            {
                parse_list(
                    parser,
                    '[',
                    |parser| {
                        parse_expression(
                            parser,
                            namespace_id,
                        )
                    },
                    |elem| {
                        args.push(elem);
                        Ok(())
                    },
                )?;
            }

            if args.len() == 0 {
                Expression {
                    pos: Some(pos),
                    kind: Node::Identifier(
                        namespace_id,
                        name,
                    ),
                }
            } else {
                Expression {
                    pos: Some(pos),
                    kind: Node::ConstCall(
                        namespace_id,
                        name,
                        args,
                    ),
                }
            }
        }
        _ => todo!("Error message for invalid value token"),
    };

    // value is an option because we need to move it to
    // a vector temporarily
    let mut value = Some(value);
    while parser.peek_token(0)?.kind
        == TokenKind::Bracket('(')
    {
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

fn parse_map(
    parser: &mut Parser,
    namespace_id: Id,
) -> ParsingResult<Expression> {
    use Error::*;
    use TokenKind::*;

    let start = parser.kind(
        Keyword("map"),
        "map has to have a keyword!",
    )?;

    let input = if !parser.try_kind(Special(":"))? {
        let input = parse_expression(parser, namespace_id)?;
        parser.kind(
            Special(":"),
            "map [expr]: [const_expr] => [value], _ => \
             [value]",
        )?;
        Some(input)
    } else {
        None
    };

    let mut branches = vec![];
    loop {
        // The default value
        if parser.try_kind(Special("_"))? {
            parser.kind(
                Special("=>"),
                "[parsing map] Got a '_', so expected the \
                 pattern '_ => [value]' for a default \
                 value, but didn't get the '=>'",
            )?;

            let value =
                parse_expression(parser, namespace_id)?;
            branches.push((None, value));
            break;
        }

        let key = parse_expression(parser, namespace_id)?;

        parser.kind(
            Special("=>"),
            "[parsing map] Didn't get a '=>' that was \
             expected. Note that you need a '_ => \
             [default value]' branch at the end to \
			close the map",
        )?;

        let value = parse_expression(parser, namespace_id)?;
        branches.push((Some(key), value));

        parser.kind(
            Special(","),
            "map needs ',' to separate elements",
        )?;
    }

    Ok(Expression {
        pos: Some(start),
        kind: Node::Map(input.map(|v| box v), branches),
    })
}

fn parse_block(
    parser: &mut Parser,
    namespace_id: Id,
) -> ParsingResult<Expression> {
    use Error::*;
    use TokenKind::*;

    let namespace_id =
        parser.create_sub_namespace(namespace_id);

    let start = parser.kind(
        TokenKind::Bracket('('),
        "( ... code here ... )",
    )?;

    let mut commands = Vec::new();
    let (is_expression, end) = loop {
        match parser.peek_token(0)? {
            Token {
                pos,
                kind: ClosingBracket('('),
            } => {
                parser.next_token();
                break (false, pos);
            }
            Token {
                kind: Keyword("const"),
                ..
            } => {
                parse_constant(parser, namespace_id)?;
                continue;
            }
            Token {
                kind: Keyword("let"),
                ..
            } => {
                let pos = parser.next_token()?.pos;
                let name = parse_identifier(
                    parser,
                    "let [name]: type = value",
                )?
                .name;

                let type_expr = if parser
                    .try_kind(TokenKind::Special(":"))?
                {
                    Some(box parse_expression(
                        parser,
                        namespace_id,
                    )?)
                } else {
                    None
                };

                parser.kind(
                    AssignOperator(""),
                    "let name: type = value;",
                )?;

                let value = box parse_expression(
                    parser,
                    namespace_id,
                )?;

                commands.push(Expression {
                    pos: Some(pos),
                    kind: Node::Declaration {
                        name,
                        type_expr,
                        value,
                    },
                });
            }
            _ => {
                let expr =
                    parse_expression(parser, namespace_id)?;

                match parser.peek_token(0)?.kind {
                    AssignOperator(op) => {
                        parser.next_token()?;
                        let right_expr = parse_expression(
                            parser,
                            namespace_id,
                        )?;

                        commands.push(Expression {
                            pos: expr.pos,
                            kind: Node::Assignment(
                                op,
                                box (expr, right_expr),
                            ),
                        });
                    }
                    _ => commands.push(expr),
                }
            }
        }

        match parser.next_token()? {
            Token {
                kind: Special(";"), ..
            } => (),
            Token {
                kind: ClosingBracket(bracket_kind),
                pos,
            } => break (true, pos),
            Token { pos, kind } => {
                return Err(InvalidToken {
                    pos,
                    got: kind,
                    pattern: "( code; code; ... code here \
                              ... )",
                    expected: vec![";", ")"],
                })
            }
        }
    };

    // TODO: If it's an expression, and there is only one
    // element in contents, we can just not have the block.
    Ok(Expression {
        pos: Some(start.join(end)),
        kind: Node::Block {
            contents: commands,
            is_expression,
        },
    })
}

/// A named or unnamed (or empty) list.
enum NamedOrUnnamedList<T> {
    Empty,
    Named(BTreeMap<TinyString, (Pos, T)>),
    Unnamed(Vec<T>),
}

impl<T> NamedOrUnnamedList<T> {
    /// If the list is named or empty, this will return the
    /// elements. If the list is unnamed, it will return an
    /// error.
    ///
    /// The ``pos`` argument is to generate a better error
    /// message.
    fn into_named(
        self,
        pos: Pos,
    ) -> ParsingResult<BTreeMap<TinyString, (Pos, T)>> {
        use Error::*;
        use NamedOrUnnamedList::*;
        match self {
            Empty => Ok(BTreeMap::new()),
            Named(map) => Ok(map),
            Unnamed(_) => Err(ExpectedNamedList { pos }),
        }
    }

    /// If the list is unnamed or empty, this will return
    /// the elements. If the list is named, it will
    /// return an error.
    ///
    /// The ``pos`` argument is to generate a better error
    /// message.
    fn into_unnamed(
        self,
        pos: Pos,
    ) -> ParsingResult<Vec<T>> {
        use Error::*;
        use NamedOrUnnamedList::*;
        match self {
            Empty => Ok(vec![]),
            Unnamed(elements) => Ok(elements),
            Named(_) => Err(ExpectedUnnamedList { pos }),
        }
    }
}

/// Parses a named or unnamed list, i.e.
/// this: ``[ a: 4.0 + 2.0, b: 5, ]``
/// or this: ``[ 4.0 + 2.0, 5, ]``.
///
/// In the named list, it also returns the positions of the
/// identifiers for the elements, because that may be
/// useful.
fn parse_named_or_unnamed_list<'a, A>(
    parser: &mut Parser<'a>,
    bracket_kind: char,
    mut parse_element: impl FnMut(
        &mut Parser<'a>,
    ) -> ParsingResult<A>,
) -> ParsingResult<(Pos, NamedOrUnnamedList<A>)> {
    use Error::*;
    use NamedOrUnnamedList::*;
    use TokenKind::*;

    let start = parser.kind(
        Bracket(bracket_kind),
        "meh this error handling system sucks, gotta make \
         it better later",
    )?;

    let mut list = Empty;
    let end = loop {
        if let Some(end) = parser
            .maybe_kind(ClosingBracket(bracket_kind))?
        {
            break end;
        }

        match (
            parser.peek_token(0)?,
            parser.peek_token(1)?,
            &mut list,
        ) {
            // Named element
            (
                Token {
                    pos,
                    kind: Identifier(name),
                },
                Token {
                    kind: Special(":"), ..
                },
                Empty,
            ) => {
                parser.next_token().unwrap();
                parser.next_token().unwrap();
                let elem = parse_element(parser)?;
                list = Named({
                    let mut map = BTreeMap::new();
                    map.insert(name, (pos, elem));
                    map
                });
            }
            (
                Token {
                    pos,
                    kind: Identifier(name),
                },
                Token {
                    kind: Special(":"), ..
                },
                Named(map),
            ) => {
                parser.next_token().unwrap();
                parser.next_token().unwrap();
                let elem = parse_element(parser)?;
                map.insert(name, (pos, elem));
            }
            (
                Token {
                    pos,
                    kind: Identifier(name),
                },
                Token {
                    kind: Special(":"), ..
                },
                Unnamed(_),
            ) => {
                return Err(NamedItemInUnnamedList { pos })
            }

            // Unnamed element
            (_, _, Empty) => {
                list = Unnamed(vec![parse_element(parser)?])
            }
            (_, _, Unnamed(list)) => {
                list.push(parse_element(parser)?)
            }
            (Token { pos, .. }, _, Named(_)) => {
                return Err(UnnamedItemInNamedList { pos })
            }
        }

        match parser.next_token()? {
            Token {
                pos,
                kind: ClosingBracket(bracket_kind),
            } => {
                break pos;
            }
            Token {
                kind: Special(","), ..
            } => (),
            Token { pos, kind } => {
                return Err(InvalidToken {
                    pos,
                    got: kind,
                    pattern: "named/unnamed list stuff,",
                    expected: vec!["}", "]", ")", ","],
                })
            }
        }
    };

    Ok((start.join(end), list))
}

fn parse_list<'a, Elem>(
    parser: &mut Parser<'a>,
    bracket_kind: char,
    mut parse_element: impl FnMut(
        &mut Parser<'a>,
    ) -> ParsingResult<Elem>,
    mut on_get_element: impl FnMut(Elem) -> ParsingResult<()>,
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
                kind:
                    TokenKind::ClosingBracket(bracket_kind),
                pos,
            } => break pos,
            Token { pos, kind } => {
                return Err(Error::InvalidToken {
                    pos,
                    got: kind,
                    pattern: "{ [name]: [item], [name]: \
                              [item] }",
                    expected: vec![",", "}"],
                })
            }
        }
    };

    Ok(start.join(end))
}

fn parse_named_list<'a, Elem>(
    parser: &mut Parser<'a>,
    bracket_kind: char,
    mut parse_element: impl FnMut(
        &mut Parser<'a>,
    ) -> ParsingResult<Elem>,
    mut on_get_element: impl FnMut(
        Identifier,
        Elem,
    ) -> ParsingResult<()>,
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
                kind:
                    TokenKind::ClosingBracket(bracket_kind),
                pos,
            } => break pos,
            Token { pos, kind } => {
                return Err(Error::InvalidToken {
                    pos,
                    got: kind,
                    pattern: "{ [name]: [item], [name]: \
                              [item] }",
                    expected: vec![",", "}"],
                })
            }
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
    DuplicateListItems {
        pos: Pos,
        old_pos: Pos,
        name: TinyString,
    },
    ExpectedNamedList {
        pos: Pos,
    },
    ExpectedUnnamedList {
        pos: Pos,
    },
    NamedItemInUnnamedList {
        pos: Pos,
    },
    UnnamedItemInNamedList {
        pos: Pos,
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
