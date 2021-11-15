use crate::symbolic::{
    value::{Constraints, Value, ConstrainedValue,
    Type, TypedValue},
};
use z3::Context;
use move_model::{
    ast::{TempIndex},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalSlot<'ctx> {
    name: String,
    ty: Type,
    content: Vec<ConstrainedValue<'ctx>>
}

impl<'ctx> LocalSlot<'ctx> {
    // set the value to v under all path constraints
    fn set_val(&mut self, v: Value<'ctx>) {
        for cv in &mut self.content {
            cv.set_val(v.clone());
        }
    }

    // load value v                    
    pub fn load(&mut self, v: TypedValue<'ctx>) {
        let (v, t) = v.decompose();
        if cfg!(debug_assertions) {
            assert_eq!(self.ty, t);
        }
        self.set_val(v);
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalState<'ctx> {
    pub context: &'ctx Context,
    slots: Vec<LocalSlot<'ctx>>,
}

impl<'ctx> LocalState<'ctx> {
    // set variable x to value v
    pub fn set_var(&mut self, x: TempIndex, v: TypedValue<'ctx>) {
        self.slots[x].load(v)
    }
}