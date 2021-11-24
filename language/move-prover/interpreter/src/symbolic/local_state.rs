use crate::symbolic::{
    value::{Constraint, Value, ConstrainedValue,
    Type, TypedValue},
};
use z3::{Context, ast::{Bool}};
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

    // self = (v_i, c_i) ...
    // produce (or (and v_i c_i) ...)
    pub fn to_condition(&self, context: &'ctx Context) -> Constraint<'ctx> {
        let conditions: Vec<Constraint> = self.content.iter().map(|x| x.condition(context)).collect();
        let conditions_ref: Vec<&Constraint> = conditions.iter().collect();
        Bool::or(context, conditions_ref.as_slice())
    }

    // self = (v_i, c_i) ...
    // produce (or (and (not v_i) c_i) ...)
    pub fn to_condition_neg(&self, context: &'ctx Context) -> Constraint<'ctx> {
        let conditions: Vec<Constraint> = self.content.iter().map(|x| x.condition_neg(context)).collect();
        let conditions_ref: Vec<&Constraint> = conditions.iter().collect();
        Bool::or(context, conditions_ref.as_slice())
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

    pub fn or(states: Vec<&LocalState<'ctx>>) -> Self {
        todo!()
    }

    pub fn get_slot(&self, index: TempIndex) -> &LocalSlot<'ctx> {
        &self.slots[index]
    }
}

pub fn merge_state<'ctx>(s1: LocalState<'ctx>, s2: LocalState<'ctx>) -> LocalState<'ctx> {
    todo!()
}