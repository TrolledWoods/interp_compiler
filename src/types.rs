use crate::prelude::*;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use std::collections::HashMap;

pub type TypeId = u64;

/// Handles type ids, to make sure that all the types
/// work the way you expect.
pub struct TypeIds {
	types: Mutex<HashMap<Type, u64>>,
    type_ctr: AtomicU64,
}

impl TypeIds {
	pub fn new() -> Self {
		let mut types = HashMap::new();
		{
			use PrimitiveKind::*;
			types.insert(Type.into(), Type.id());
			types.insert(Float32.into(), Float32.id());
			types.insert(Float64.into(), Float64.id());
			types.insert(Char32.into(), Char32.id());
			types.insert(Char64.into(), Char64.id());
		}

		TypeIds {
			types: Mutex::new(types),
			type_ctr: AtomicU64::new(6),
		}
	}

	pub fn type_to_id(&self, t: &Type) -> u64 {
		let mut types = self.types.lock().unwrap();
		if let Some(&id) = types.get(t) {
			return id;
		}

		let id = self.type_ctr.fetch_add
			(1, Ordering::SeqCst);
		types.insert(t.clone(), id);
		id
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PrimitiveKind {
    Char32,
    Char64,
    Float32,
    Float64,
    Type,
	Empty,
}

impl PrimitiveKind {
	#[inline(always)]
	pub fn id(self) -> TypeId {
		match self {
			PrimitiveKind::Type => 0,
			PrimitiveKind::Char32 => 1,
			PrimitiveKind::Char64 => 2,
			PrimitiveKind::Float32 => 3,
			PrimitiveKind::Float64 => 4,
			PrimitiveKind::Empty => 5,
		}
	}
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub cachable: bool,
}

impl From<PrimitiveKind> for Type {
	fn from(primitive: PrimitiveKind) -> Self {
		let cachable = {
			use PrimitiveKind::*;
			match primitive {
				Empty | Char32 | Char64 | Type => true,
				Float32 | Float64 => false,
			}
		};

		Type {
			kind: TypeKind::Primitive(primitive),
			cachable,
		}
	}
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Unique(u32, TypeId),
    Primitive(PrimitiveKind),
    FixedArray(usize, TypeId),
    Collection(Vec<(TinyString, TypeId)>),
    Pointer(TypeId),
}
