use z3::ast as zast;
use z3::{Context, ast::{Bool, Int, Ast}};
use bytecode::{
    stackless_bytecode::{Constant},
};
use std::fmt;

pub type Constraint<'ctx> = zast::Bool<'ctx>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value<'ctx> {
    Bool(zast::Bool<'ctx>),
    Int(zast::Int<'ctx>),
    Undefined,
    // ...
}

impl<'ctx> Value<'ctx> {
    /// Produces the value corresponding to constant `c`.
    pub fn from_constant(c: &Constant, context: &'ctx Context) -> Self {
        match c {
            Constant::Bool(b) => Value::Bool(Bool::from_bool(context, b.clone())),
            Constant::U8(x) => Value::Int(Int::from_u64(context, *x as u64)),
            Constant::U64(x) => Value::Int(Int::from_u64(context, *x as u64)),
            Constant::U128(x) => Value::Int(Int::from_u64(context, *x as u64)),
            _ => todo!(),
        }
    }

    pub fn mk_bool(b: bool, ctx: &'ctx Context) -> Self {
        Self::Bool(zast::Bool::from_bool(ctx, b))
    }

    pub fn mk_int(u: u64, ctx: &'ctx Context) -> Self {
        Self::Int(zast::Int::from_u64(ctx, u))
    }

    pub fn as_z3_bool(&self) -> &Bool<'ctx> {
        match self {
            Value::Bool(b) => b,
            _ => panic!(),
        }
    }

    pub fn get_context(&self) -> &'ctx Context {
        match self {
            Self::Bool(x) => x.get_ctx(),
            Self::Int(x) => x.get_ctx(),
            _ => todo!(),
        }
    }

    /// Produces `self + other`. Panics if `self` or `other` are not nubmers.
    pub fn add(&self, other: &Value<'ctx>) -> Self {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
            _ => panic!("Wrong type for addition!"),
        }
    }
    
    // Produces `self < other`. Panics if types mismatch.
    pub fn lt(&self, other: &Value<'ctx>) -> Self {
        match (self, other) {
            (Value::Int(x), Value::Int(y)) => Value::Bool(x.lt(y)),
            _ => panic!("Wrong type for comparison!"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum IntType {
    Num,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum PrimitiveType {
    Bool,
    Int(IntType),
    Address,
    Signer,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum BaseType {
    Primitive(PrimitiveType),
    Vector(Box<BaseType>),
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Type {
    Base(BaseType),
    Reference(bool, BaseType),
}

impl Type {
    pub fn mk_bool() -> Self {
        Type::Base(BaseType::Primitive(PrimitiveType::Bool))
    }

    pub fn mk_num() -> Self {
        Type::Base(BaseType::Primitive(PrimitiveType::Int(IntType::Num)))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedValue<'ctx> {
    val: Value<'ctx>,
    ty: Type,
}

impl<'ctx> TypedValue<'ctx> {
    pub fn get_val(&self) -> &Value {
        &self.val
    }

    pub fn get_ty(&self) -> &Type {
        &self.ty
    }

    pub fn decompose(self) -> (Value<'ctx>, Type) {
        (self.val, self.ty)
    }

    pub fn mk_bool(v: bool, ctx: &'ctx Context) -> Self {
        TypedValue{
            ty: Type::mk_bool(),
            val: Value::mk_bool(v, ctx),
        }
    }

    pub fn mk_u8(v: u8, ctx: &'ctx Context) -> Self {
        todo!();
    }
}

/// A value under some path constraint.
#[derive(Clone, Eq, PartialEq)]
pub struct ConstrainedValue<'ctx> {
    value: Value<'ctx>,
    constraint: Constraint<'ctx>,
}

impl<'ctx> fmt::Debug for ConstrainedValue<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("{:?} if {:?}", &self.value, &self.constraint))
    }
}

impl<'ctx> ConstrainedValue<'ctx> {
    pub fn new(value: Value<'ctx>, constraint: Constraint<'ctx>) -> Self {
        Self { value, constraint }
    }

    pub fn from_value(value: Value<'ctx>, context: &'ctx Context) -> Self {
        Self { value, constraint: Bool::from_bool(context, true) }
    }
    
    pub fn decompose(self) -> (Value<'ctx>, Constraint<'ctx>) {
        (self.value, self.constraint)
    }

    pub fn set_val(&mut self, v: Value<'ctx>) {
        self.value = v;
    }

    pub fn add_constraint(&mut self, p: &Constraint<'ctx>, ctx: &'ctx Context) {
        let constraints = vec![&self.constraint, p];
        self.constraint = Bool::and(ctx, constraints.as_slice());
    }

    pub fn condition(&self, context: &'ctx Context) -> Constraint<'ctx> {
        Bool::and(context, vec![self.value.as_z3_bool(), &self.constraint].as_slice())
    }

    pub fn condition_neg(&self, context: &'ctx Context) -> Constraint<'ctx> {
        Bool::and(context, vec![&self.value.as_z3_bool().not(), &self.constraint].as_slice())
    }
}