use crate::symbolic::{
    value::{Constraint, Value, ConstrainedValue,
    Type, TypedValue},
};
use z3::{Context, ast::{Bool, Ast}, Solver, SatResult};
use move_model::{
    ast::{TempIndex},
};
use bytecode::{
    stackless_bytecode::{Constant},
};
use std::fmt;

#[derive(Clone, Eq, PartialEq)]
pub struct LocalSlot<'ctx> {
    name: String,
    ty: Type,
    content: Vec<ConstrainedValue<'ctx>>,
    context: &'ctx Context,
}

impl<'ctx> fmt::Debug for LocalSlot<'ctx> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_fmt(format_args!("{} = {:?}", self.name, self.content))
    }
}

impl<'ctx> LocalSlot<'ctx> {
    /// Produces a slot with an undefined value of type `t`.
    pub fn from_type(t: Type, ctx: &'ctx Context) -> Self {
        Self {
            name: String::new(),
            ty: t,
            content: vec![ConstrainedValue::new(Value::Undefined, Bool::from_bool(ctx, true))],
            context: ctx,
        }
    }

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

    /// Loads constant `c` in current slot. Panics if type mismatches.
    // todo: check for type mismatches.
    pub fn load_constant(&mut self, c: &Constant) {
        let val = Value::from_constant(c, self.context);
        // todo
        for cv in &mut self.content {
            cv.set_val(val.clone());
        }
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

    pub fn add_constraint(&mut self, p: &Constraint<'ctx>, ctx: &'ctx Context) {
        for cv in &mut self.content {
            cv.add_constraint(p, ctx);
        }
    }

    pub fn merge(mut s1: Self, mut s2: Self) -> Self {
        assert_eq!(s1.name, s2.name);
        assert_eq!(s1.ty, s2.ty);
        s1.content.append(&mut s2.content);
        s1
    }

    /// Sets the slot content to empty, and returns the original value.
    pub fn del_val(&mut self) -> Vec<ConstrainedValue<'ctx>> {
        let res = self.content.clone();
        self.content = Vec::new();
        res
    }

    /// Procudes the content of the slot.
    pub fn get_val(&self) -> Vec<ConstrainedValue<'ctx>> {
        self.content.clone()
    }

    /// Sets the content of the slot to `vals`.
    pub fn set(&mut self, vals: Vec<ConstrainedValue<'ctx>>) {
        self.content = vals;
    }

    pub fn set_name(&mut self, s: String) {
        self.name = s;
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct LocalState<'ctx> {
    slots: Vec<LocalSlot<'ctx>>,
}

impl<'ctx> fmt::Debug for LocalState<'ctx> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_fmt(format_args!("{:?}", self.slots))
    }
}

impl<'ctx> LocalState<'ctx> {
    pub fn from_types(types: &[Type], context: &'ctx Context) -> Self {
        let mut counter = 0;
        Self {
            slots: types.iter().map(|x| {
                let mut res = LocalSlot::from_type(x.clone(), context);
                res.set_name(format!("$t{}", counter));
                counter = counter + 1;
                res
            }).collect()
        }
    }

    // set variable x to value v
    pub fn set_var(&mut self, x: TempIndex, v: TypedValue<'ctx>) {
        self.slots[x].load(v)
    }

    /// Sets `vars[i]` to `vals[i]`. Panics if `vars` and `vals` have different lengths.
    pub fn set_vars(&mut self, vars: &[TempIndex], vals: Vec<Vec<ConstrainedValue<'ctx>>>) {
        assert_eq!(vars.len(), vals.len());
        for (var, val) in vars.iter().zip(vals.into_iter()) {
            self.slots.get_mut(*var).unwrap().set(val);
        }
    }

    // Sets the values of variable `x` to `vals`.
    pub fn set(&mut self, x: TempIndex, vals: Vec<ConstrainedValue<'ctx>>) {
        self.slots.get_mut(x).unwrap().set(vals)
    }

    pub fn add_constraint(&mut self, p: &Constraint<'ctx>, ctx: &'ctx Context) {
        for slot in &mut self.slots {
            slot.add_constraint(p, ctx);
        }
    }

    pub fn get_slot(&self, index: TempIndex) -> &LocalSlot<'ctx> {
        &self.slots[index]
    }

    pub fn get_mut_slot(&mut self, index: TempIndex) -> &mut LocalSlot<'ctx> {
        &mut self.slots[index]
    }

    pub fn merge(s1: LocalState<'ctx>, s2: LocalState<'ctx>) -> LocalState<'ctx> {
        Self {
            slots: {
                s1.slots.into_iter().zip(s2.slots.into_iter())
                  .map(|(l1, l2)| LocalSlot::merge(l1, l2)).collect()
            }
        }
    }

    /// Deletes and produces the values of variable `index`.
    pub fn del_val(&mut self, index: TempIndex) -> Vec<ConstrainedValue<'ctx>> {
        self.slots.get_mut(index).unwrap().del_val()
    }

    /// Procudes the values of variable `index`.
    pub fn get_val(&self, index: TempIndex) -> Vec<ConstrainedValue<'ctx>> {
        self.slots.get(index).unwrap().get_val()
    }

    /// Produces all possible (constrained values) of (v_1, ..., v_n)
    /// for v_i in `vars`.
    pub fn get_constrained_operands(&self, vars: &[TempIndex]) -> Vec<(Vec<Value<'ctx>>, Constraint<'ctx>)> {
        // val = (v, p), vals = ((w_1, ..., w_n), q) ...
        // produces ((w_1, ..., w_n, v), p /\ q) ... for all p /\ q not unsatisfiable
        fn add_operand<'ctx>(val: ConstrainedValue<'ctx>, vals: Vec<(Vec<Value<'ctx>>, Constraint<'ctx>)>) -> Vec<(Vec<Value<'ctx>>, Constraint<'ctx>)> {
            if vals.is_empty() {
                let (value, constraint) = val.decompose();
                return vec![(vec![value], constraint)];
            }
            let mut res: Vec<(Vec<Value<'ctx>>, Constraint<'ctx>)> = Vec::new();
            let (value, constraint) = val.decompose();
            let context = constraint.get_ctx();
            for mut v in vals {
                let solver = Solver::new(context);
                solver.assert(&constraint);
                solver.assert(&v.1);
                match solver.check() {
                    SatResult::Unsat => (),
                    _ => {
                        v.0.push(value.clone());
                        res.push((v.0, Bool::and(context, &vec![&constraint, &v.1])));
                    }
                }
            }
            res
        }
        if vars.is_empty() {
            Vec::new()
        } else {
            let (last, firsts) = vars.split_last().unwrap();
            let almost = self.get_constrained_operands(firsts);
            self.get_val(*last).into_iter().map(|x| add_operand(x, almost.clone())).flatten().collect()
        }
    }
}
