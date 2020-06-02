use crate::prelude::*;
use std::collections::VecDeque;
use std::io;
use std::path::Path;
use std::str::Chars;

pub type LexingResult<T> = Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Error {
    EndOfFile,
    UnknownCharacter(Pos, char),
    BaseBeforeNumber(Pos),
    BaseOutOfBounds(Pos, u128),
    ExpectedNumber(Pos),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub pos: Pos,
    pub kind: TokenKind,
}

impl Token {
    pub fn into_parts(self) -> (Pos, TokenKind) {
        (self.pos, self.kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Bracket(char),
    /// Closing brackets are reversed, i.e.
    /// a ``')'`` becomes ``ClosingBracket('(')``
    ClosingBracket(char),
    Identifier(TinyString),
    Operator(&'static str),
    AssignOperator(&'static str),
    Special(&'static str),
    Keyword(&'static str),
    Primitive(PrimitiveKind),
    IntLiteral(u128),
    FloatLiteral(f64),
    /// Maybe use a ``Arc<String>`` here? Or have a
    /// ``BigStringLiteral(Arc<String>)`` for really
    /// long strings.
    StringLiteral(TinyString),
}

impl From<Number> for TokenKind {
    fn from(number: Number) -> Self {
        match number {
            Number::Integer(v) => TokenKind::IntLiteral(v),
            Number::Float(v) => TokenKind::FloatLiteral(v),
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    file: TinyString,
    source: Chars<'a>,
    cached_tokens: VecDeque<Result<Token, Error>>,
    pos: (usize, usize),
}

impl Lexer<'_> {
    pub fn new<'a>(
        file: TinyString,
        source: &'a str,
    ) -> Lexer<'a> {
        Lexer {
            cached_tokens: VecDeque::new(),
            source: source.chars(),
            pos: (0, 0),
            file,
        }
    }

    pub fn at_end_of_file(&mut self) -> bool {
        self.peek_token(0).is_err()
    }

    pub fn peek_token<'a>(
        &'a mut self,
        n_forward: usize,
    ) -> LexingResult<Token> {
        if self.cached_tokens.get(n_forward).is_some() {
            return self.cached_tokens[n_forward].clone();
        }

        while self.cached_tokens.len() <= n_forward {
            self.cached_tokens.push_back(read_token(
                &mut self.source,
                self.file,
                &mut self.pos,
            ));
        }

        self.cached_tokens.get(n_forward).unwrap().clone()
    }

    pub fn next_token(&mut self) -> LexingResult<Token> {
        self.cached_tokens.pop_front().unwrap_or_else(
            || {
                read_token(
                    &mut self.source,
                    self.file,
                    &mut self.pos,
                )
            },
        )
    }
}

fn read_token(
    chars: &mut Chars,
    file: TinyString,
    pos: &mut (usize, usize),
) -> Result<Token, Error> {
    skip_whitespace(chars, pos);

    while chars.as_str().starts_with("//") {
        // Skip characters until the next newline
        'inner: while let Some(c) = chars.next() {
            if c == '\n' {
                break 'inner;
            }
        }

        skip_whitespace(chars, pos);
    }

    let start = *pos;

    match chars.clone().next() {
        Some(c) if c.is_alphabetic() || c == '_' => {
            for (name, token_kind) in RESERVED_WORDS {
                if chars.as_str().starts_with(name) {
                    match chars.as_str()[name.len()..]
                        .chars()
                        .next()
                    {
                        Some(c)
                            if c.is_alphanumeric()
                                || c == '_' =>
                        {
                            ()
                        }
                        _ => {
                            let start = *pos;
                            skip_n_chars(
                                chars,
                                pos,
                                name.len(),
                            );
                            println!("{}", name);
                            return Ok(Token {
                                pos: Pos::new(
                                    file, start, *pos,
                                ),
                                kind: token_kind.clone(),
                            });
                        }
                    }
                }
            }
        }
        Some(c) => {
            for (name, token_kind) in RESERVED_CHARACTERS {
                if chars.as_str().starts_with(name) {
                    let start = *pos;
                    skip_n_chars(chars, pos, name.len());
                    return Ok(Token {
                        pos: Pos::new(file, start, *pos),
                        kind: token_kind.clone(),
                    });
                }
            }
        }
        None => return Err(Error::EndOfFile),
    }

    match peek_chars(chars, 0) {
        Some(c) if c.is_alphabetic() || c == '_' => {
            let (ident_pos, name) =
                read_identifier(chars, file, pos)?;

            return Ok(Token {
                pos: ident_pos,
                kind: TokenKind::Identifier(name),
            });
        }
        Some(c) if c.is_digit(10) || c == '.' => {
            let (number_pos, number) =
                read_number(chars, file, pos, true)?;

            return Ok(Token {
                pos: number_pos,
                kind: number.into(),
            });
        }
        Some('"') => {
            todo!("String literals!");
        }
        Some(c) => {
            return Err(Error::UnknownCharacter(
                Pos::single(file, *pos),
                c,
            ))
        }
        None => return Err(Error::EndOfFile),
    }
}

enum Number {
    /// All literals are unsized, negative values
    /// can be acquired by using minus unary
    /// operators.
    Integer(u128),
    Float(f64),
}

/// Reads an identifier.
fn read_identifier(
    chars: &mut Chars,
    file: TinyString,
    pos: &mut (usize, usize),
) -> LexingResult<(Pos, TinyString)> {
    let string = chars.as_str();
    let mut peek_chars = string.char_indices();
    let start = *pos;

    let mut length = None;
    let mut n_chars = 0;
    while let Some((i, c)) = peek_chars.next() {
        if c.is_alphanumeric() || c == '_' {
            length = Some(i);
            n_chars += 1;
        } else {
            break;
        }
    }
    skip_n_chars(chars, pos, n_chars);

    let end = *pos;
    let length = length.expect(
        "Don't call the read_identifier function when
        you don't have an identifier",
    );

    Ok((
        Pos::new(file, start, end),
        string[..=length].into(),
    ))
}

fn read_number(
    chars: &mut Chars,
    file: TinyString,
    pos: &mut (usize, usize),
    allow_float: bool,
) -> LexingResult<(Pos, Number)> {
    let mut peek_chars = chars.clone();
    let mut number: Option<u128> = None;
    let mut n_chars = 0;
    let mut base: Option<u32> = None;

    let start = *pos;

    let mut float_offset: Option<i32> = None;
    while let Some(c) = peek_chars.next() {
        match (c, c.to_digit(base.unwrap_or(10))) {
            (_, Some(digit)) => {
                float_offset = float_offset.map(|v| v + 1);

                let mut n = number.unwrap_or(0);
                n = n * base.unwrap_or(10) as u128
                    + digit as u128;
                number = Some(n);
            }
            // Your own base
            ('b', _) => {
                if base.is_some() {
                    break;
                }

                let number =
                    number.take().ok_or_else(|| {
                        Error::BaseBeforeNumber(
                            Pos::single(file, *pos),
                        )
                    })?;

                if !(2..=36).contains(&number) {
                    return Err(Error::BaseOutOfBounds(
                        Pos::new(file, start, *pos),
                        number,
                    ));
                }

                // This is safe because we know that
                // number is <= 36
                base = Some(number as u32);
            }
            ('.', _) => {
                // Floating point
                if !allow_float || float_offset.is_some() {
                    break;
                }

                float_offset = Some(0);
            }
            (_, _) => break,
        }

        n_chars += 1;
    }

    skip_n_chars(chars, pos, n_chars);
    let end = *pos;

    let number = number.ok_or_else(|| {
        Error::ExpectedNumber(Pos::single(file, *pos))
    })?;

    let number = match float_offset {
        Some(offset) => Number::Float(
            (number as f64)
                / (base.unwrap_or(10) as f64).powi(offset),
        ),
        None => Number::Integer(number),
    };

    Ok((Pos::new(file, start, end), number))
}

#[inline(always)]
fn peek_chars(
    chars: &Chars,
    n_chars: usize,
) -> Option<char> {
    let mut chars = chars.clone();

    // Skip the first few
    for _ in 0..n_chars {
        chars.next();
    }

    chars.next()
}

fn skip_n_chars(
    chars: &mut Chars,
    pos: &mut (usize, usize),
    n_chars: usize,
) {
    for _ in 0..n_chars {
        let next = chars
            .next()
            .expect("skipped more chars than there are");
        *pos = increment_pos(*pos, next);
    }
}

fn skip_whitespace(
    chars: &mut Chars,
    pos: &mut (usize, usize),
) {
    let mut n_whitespace_chars = 0;
    let mut chars_copy = chars.clone();
    loop {
        match chars_copy.next() {
            Some(c) if c.is_whitespace() => {
                n_whitespace_chars += 1
            }
            _ => break,
        }
    }

    skip_n_chars(chars, pos, n_whitespace_chars);
}

/// Only increments the character of the position.
fn increment_char(pos: (usize, usize)) -> (usize, usize) {
    (pos.0, pos.1 + 1)
}

/// Increments the position.
/// The char is needed to handle newlines.
fn increment_pos(
    pos: (usize, usize),
    incrementor: char,
) -> (usize, usize) {
    match incrementor {
        '\n' => (pos.0 + 1, 0),
        _ => (pos.0, pos.1 + 1),
    }
}

/// Alphbetic words.
const RESERVED_WORDS: &[(&str, TokenKind)] = {
    use TokenKind::*;
    &[
        // Keywords
        ("fn", Keyword("fn")),
        ("mod", Keyword("mod")),
        ("for", Keyword("for")),
        ("if", Keyword("if")),
        ("else", Keyword("else")),
        ("unique", Keyword("unique")),
        ("const", Keyword("const")),
        ("let", Keyword("let")),
        ("map", Keyword("map")),
        // Special things
        ("_", Special("_")),
        // Primitive types
        ("char_32", Primitive(PrimitiveKind::Char32)),
        ("char_64", Primitive(PrimitiveKind::Char64)),
        ("type", Primitive(PrimitiveKind::Type)),
    ]
};

// TODO: Optimize this by making some tree
// structure that allows for more efficient
// tokenization of things(but remember to
// profile it too, please!)
/// Non alphabetic reserved words that directly map to
/// tokens.
///
/// The difference between this and ``RESERVED_WORDS``,
/// is that words cannot be joined together with other
/// words, while these special tokens can be joined together
/// with other special tokens.
///
/// Example: ``for_blah : f32 = 0.4;`` is a valid statement,
/// because ``for_blah`` is a single identifier, not ``for``
/// then ``_blah``. ``x +- 3`` is also valid, but here
/// because ``+-`` is turned into ``+`` and ``-``. We need
/// both of these different behaviours, so we cannot put
/// them into the same list.
const RESERVED_CHARACTERS: &[(&str, TokenKind)] = {
    use TokenKind::*;
    &[
        ("+=", AssignOperator("+")),
        ("-=", AssignOperator("-")),
        ("*=", AssignOperator("*")),
        ("/=", AssignOperator("/")),
        ("<=", Operator("<=")),
        (">=", Operator(">=")),
        ("==", Operator("==")),
        ("&&", Operator("&&")),
        ("||", Operator("||")),
        ("=>", Special("=>")),
        ("=", AssignOperator("")),
        ("/", Operator("/")),
        ("+", Operator("+")),
        ("-", Operator("-")),
        ("*", Operator("*")),
        ("<-", Operator("<-")),
        ("[-]", Special("[-]")),
        ("[?]", Special("[?]")),
        ("<", Operator("<")),
        (">", Operator(">")),
        (":", Special(":")),
        (";", Special(";")),
        (",", Special(",")),
        // Brackets
        ("(", Bracket('(')),
        (")", ClosingBracket('(')),
        ("{", Bracket('{')),
        ("}", ClosingBracket('{')),
        ("[", Bracket('[')),
        ("]", ClosingBracket('[')),
    ]
};

// I know all things are not covered by the tests,
// but I feel like this is fine.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lines() {
        let mut lexer = Lexer::new(
            "num".into(),
            r#"
x
y z
w
"#,
        );
        assert_eq!(
            lexer.next_token().unwrap().pos,
            (1, 0)..(1, 1)
        );
        assert_eq!(
            lexer.next_token().unwrap().pos,
            (2, 0)..(2, 1)
        );
        assert_eq!(
            lexer.next_token().unwrap().pos,
            (2, 2)..(2, 3)
        );
        assert_eq!(
            lexer.next_token().unwrap().pos,
            (3, 0)..(3, 1)
        );
    }

    #[test]
    fn no_errors() {
        let mut lexer = Lexer::new(
            "num".into(),
            "x := 23.0; y := 15; print(25); [2, 3, 4]",
        );

        while match lexer.next_token() {
            Ok(_) => true,
            Err(Error::EndOfFile) => false,
            Err(err) => panic!("Got error {:?}", err),
        } {}
    }

    #[test]
    fn lex_number() {
        let mut lexer =
            Lexer::new("num".into(), "2b101 23.5 3b21.1");

        let token = lexer.next_token().unwrap();
        // assert_eq!(token.pos, (0, 0)..(0, 5));
        assert_eq!(TokenKind::IntLiteral(5), token.kind,);

        let token = lexer.next_token().unwrap();
        assert_eq!(token.pos, (0, 6)..(0, 10));
        if let TokenKind::FloatLiteral(v) = token.kind {
            assert!((v - 23.5).abs() <= 1e-10);
        } else {
            panic!("Expected float");
        }

        let token = lexer.next_token().unwrap();
        assert_eq!(token.pos, (0, 11)..(0, 17));
        if let TokenKind::FloatLiteral(v) = token.kind {
            assert!(
                (v - (2.0 * 3.0 + 1.0 + 1.0 / 3.0)).abs()
                    <= 1e-10
            );
        } else {
            panic!("Expected float");
        }
    }

    #[test]
    fn lex_identifier() {
        let mut lexer =
            Lexer::new("hello".into(), "for_+max");

        let token = lexer.next_token().unwrap();
        assert_eq!(
            token.kind,
            TokenKind::Identifier("for_".into())
        );
        assert_eq!(token.pos, (0, 0)..(0, 4));

        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, TokenKind::Operator("+"));
        assert_eq!(token.pos, (0, 4)..(0, 5));

        let token = lexer.next_token().unwrap();
        assert_eq!(
            token.kind,
            TokenKind::Identifier("max".into())
        );
        assert_eq!(token.pos, (0, 5)..(0, 8));
    }

    #[test]
    fn lex_keyword() {
        let mut lexer =
            Lexer::new("hello".into(), "for,if");

        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, TokenKind::Keyword("for"));
        assert_eq!(token.pos, (0, 0)..(0, 3));

        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, TokenKind::Special(","));
        assert_eq!(token.pos, (0, 3)..(0, 4));
    }

    #[test]
    fn lex_special_characters() {
        let mut lexer = Lexer::new("hello".into(), "+=[]");

        let token = lexer.next_token().unwrap();
        assert_eq!(
            token.kind,
            TokenKind::AssignOperator("+")
        );
        assert_eq!(token.pos, (0, 0)..(0, 2));

        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, TokenKind::Bracket('['));
        assert_eq!(token.pos, (0, 2)..(0, 3));
    }
}
