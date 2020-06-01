use crate::prelude::*;
use std::collections::BTreeMap;

/// A collection of useful things to have
/// from this module.
pub mod prelude {
    pub use super::{CollectionElement, Expression, Node};
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub pos: Option<Pos>,
    pub kind: Node,
}

#[derive(Debug, Clone)]
pub enum Node {
    /// A collection can be either a type or a value!
    /// They are both the same!
    /// A type collection will obviously convert into a
    /// type id(like all types).
    Collection(BTreeMap<TinyString, (Pos, Expression)>),

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

    /// The first expression is the l-value, the right
    /// expression is the r-value. Yes, there is no
    /// differenciation. That happens when
    /// compiling/interpreting.
    Assignment(Box<(Expression, Expression)>),

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

#[derive(Debug, Clone)]
pub struct CollectionElement {
    pub value: Expression,

    /// Should only be ``Some`` if the collection is
    /// a type collection.
    pub default: Option<Expression>,
}
