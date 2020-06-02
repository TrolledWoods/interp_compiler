use crate::prelude::*;
use crate::parser::expression::prelude::*;
use std::collections::BTreeMap;

pub mod prelude {
	pub use super::Context as InterpContext;
	pub use super::interp;
}

pub type InterpResult<T> = Result<T, Error>;

#[derive(Clone, Copy)]
pub struct Context<'a> {
	pub type_ids: &'a TypeIds,
}

impl Context<'_> {
	#[inline(always)]
	fn type_to_id(&self, t: &Type) -> u64 {
		self.type_ids.type_to_id(t)
	}
}

/// Interprets an expression.
pub fn interp(
	context: Context,
	expr: &mut Expression,
	get_namespace_item: 
		&mut impl FnMut(Id, TinyString) 
			-> GetResult<Value>,
) -> InterpResult<Value> {
	match &mut expr.kind {
		Node::Collection(map) => {
			let mut type_data = BTreeMap::new();
			let mut is_type = true;

			for (key, (pos, value)) in map.iter_mut() {
				let value = interp(
					context,
					value, 
					get_namespace_item,
				)?;
				type_data.insert(
					*key, 
					value._type.clone(),
				);
			}
			
			todo!();
		}
		Node::Primitive(kind) => {
			Ok(Value {
				_type: PrimitiveKind::Type.into(),
				value: vec![Primitive::Type(kind.id())],
			})
		}
		_ => todo!("not done"),
	}
}

pub type GetResult<T> = Result<T, GetError>;

#[derive(Debug)]
pub enum GetError {
	Poisoned,
	UnknownName,
}

#[derive(Debug)]
pub enum Error {
	Poisoned,
	NamespaceItemNotFound(Pos, Id, TinyString),
}

pub enum Primitive {
	Float32(f32),
	Float64(f64),
	Char32(u32),
	Char64(u64),
	Type(u64),
}

pub struct Value {
	_type: Type,
	value: Vec<Primitive>,
}
