use crate::prelude::*;
use std::sync::atomic::{AtomicU32, Ordering};

pub type TypeId = u32;

/// Handles type ids, to make sure that all the types
/// work the way you expect.
pub struct TypeIds {
    unique_type_ctr: AtomicU32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PrimitiveKind {
    Char8,
    Char16,
    Char32,
    Char64,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Type,
}

pub struct Type {
    pub kind: TypeKind,
    pub cachable: bool,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Unique(u32, TypeId),
    Primitive(PrimitiveKind),
    FixedArray(usize, TypeId),
    Collection(Vec<(TinyString, TypeId)>),
    Pointer(TypeId),
}
