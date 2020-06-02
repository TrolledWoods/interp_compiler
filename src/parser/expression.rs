use crate::prelude::*;
use std::collections::BTreeMap;
use std::fmt;

/// A collection of useful things to have
/// from this module.
pub mod prelude {
    pub use super::{CollectionElement, Expression, Node};
}

#[derive(Clone)]
pub struct Expression {
    pub pos: Option<Pos>,
    pub kind: Node,
}

#[derive(Clone)]
pub enum Node {
    /// A collection can be either a type or a value!
    /// They are both the same!
    /// A type collection will obviously convert into a
    /// type id(like all types).
    Collection(BTreeMap<TinyString, (Pos, Expression)>),

    /// Unreachable expression
    Unreachable,

    /// Just an identifier.
    Identifier(Id, TinyString),
    /// A primitive type
    Primitive(PrimitiveKind),

    UnaryOperator(&'static str, Box<Expression>),
    BinaryOperator(
        &'static str,
        Box<(Expression, Expression)>,
    ),

    /// A constant function call, with the given
    /// expressions as input. Maybe make named
    /// parameters a thing later on?
    ConstCall(Id, TinyString, Vec<Expression>),

    /// A function call. The first element of the vector
    /// is the function you're calling, which can be an
    /// arbitrary complex expression, of course. The rest
    /// are the function arguments in order.
    FunctionCall(Vec<Expression>),

    /// A declaration. This will return some weird type,
    /// that can never be used. So this behaves as a
    /// statement, because it cannot be used as an
    /// expression.
    Declaration {
        name: TinyString,
        type_expr: Option<Box<Expression>>,
        value: Box<Expression>,
    },

    /// The main branching system in this language.
    Map(
        Option<Box<Expression>>,
        Vec<(Option<Expression>, Expression)>,
    ),

    /// The first expression is the l-value, the right
    /// expression is the r-value. Yes, there is no
    /// differenciation. That happens when
    /// compiling/interpreting.
    Assignment(&'static str, Box<(Expression, Expression)>),

    IntLiteral(u128),
    FloatLiteral(f64),

    /// A set of expressions.
    /// If the ``returns_something`` flag is set, the block
    /// will return the value of the last expression in
    /// the block.
    Block {
        contents: Vec<Expression>,
        is_expression: bool,
    },
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Node::*;
        match self {
            Collection(map) => f
                .debug_map()
                .entries(
                    map.iter().map(|(k, (v1, v2))| (k, v2)),
                )
                .finish(),
            Map(input, elements) => {
                write!(f, "map")?;
                if let Some(input) = input {
                    write!(f, " {:?}", input)?;
                }
                write!(f, ":")?;
                f.debug_map()
                    .entries(
                        elements
                            .iter()
                            .map(|(k, v)| (k, v)),
                    )
                    .finish()
            }
            Unreachable => unreachable!(),
            Identifier(id, tiny) => {
                write!(f, "{}:{}", id, tiny)
            }
            Primitive(kind) => write!(f, "{:?}", kind),
            UnaryOperator(op, expr) => {
                write!(f, "{}{:?}", op, expr)
            }
            BinaryOperator(op, box (a, b)) => {
                write!(f, "({:?} {} {:?})", a, op, b)
            }
            ConstCall(id, name, args) => {
                write!(f, "{}:{}{:?}", id, name, args)
            }
            FunctionCall(args) => {
                let mut args = args.iter();
                write!(f, "{:?}(", args.next().unwrap())?;
                for (i, arg) in args.enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", arg)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Declaration {
                name,
                type_expr,
                value,
            } => {
                write!(f, "let {}", name)?;
                if let Some(expr) = type_expr {
                    write!(f, ": {:?}", expr)?;
                }
                write!(f, " = {:?}", value)?;
                Ok(())
            }
            Assignment(op, box (left, right)) => {
                write!(f, "{:?} {}= {:?}", left, op, right)
            }
            IntLiteral(num) => write!(f, "{}", num),
            FloatLiteral(num) => write!(f, "{}", num),
            Block {
                contents,
                is_expression,
            } => {
                let mut debug = if *is_expression {
                    f.debug_tuple("expr_block")
                } else {
                    f.debug_tuple("block")
                };

                for content in contents {
                    debug.field(content);
                }

                debug.finish()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CollectionElement {
    pub value: Expression,

    /// Should only be ``Some`` if the collection is
    /// a type collection.
    pub default: Option<Expression>,
}
