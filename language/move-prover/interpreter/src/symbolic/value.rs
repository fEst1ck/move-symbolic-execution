use z3::ast as zast;
use z3::Context;

pub type Constraint<'ctx> = zast::Bool<'ctx>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constraints<'ctx> {
    constraint: Vec<Constraint<'ctx>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value<'ctx> {
    Bool(zast::Bool<'ctx>),
    Int(zast::Int<'ctx>),
    // ...
}

impl<'ctx> Value<'ctx> {
    pub fn mk_bool(b: bool, ctx: &'ctx Context) -> Self {
        Self::Bool(zast::Bool::from_bool(ctx, b))
    }

    pub fn mk_int(u: u64, ctx: &'ctx Context) -> Self {
        Self::Int(zast::Int::from_u64(ctx, u))
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstrainedValue<'ctx> {
    value: Value<'ctx>,
    constraints: Constraints<'ctx>,
}

impl<'ctx> ConstrainedValue<'ctx> {
    pub fn set_val(&mut self, v: Value<'ctx>) {
        self.value = v;
    }
}