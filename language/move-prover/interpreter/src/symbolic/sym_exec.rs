use move_stackless_bytecode::{
  stackless_bytecode::{
    Bytecode, Label, Constant, AssignKind, Operation,
    AbortAction,
  },
  stackless_control_flow_graph::{
    StacklessControlFlowGraph,
    BlockContent,
  },
};
use std::collections::{BTreeSet};
use crate::symbolic::local_state::{
  LocalState,
};
use crate::symbolic::value::{
  TypedValue,
  Constraint,
  ConstrainedValue,
};
use move_model::{
  ast::{TempIndex},
};
use z3::{ast::{Bool}, Context};

fn is_jump_instruction(stmt: &Bytecode) -> bool {
  match stmt {
    Bytecode::Jump(_, _) => true,
    Bytecode::Branch(_, _, _, _) => true,
    _ => false,
  }
}

// Evaluates a jump or branch instruction, and return the path constraints of
// the out-edges. Panics if given non-branch instruction.
fn eval_jump<'ctx>(stmt: &Bytecode, s: &LocalState<'ctx>, context: &'ctx Context) -> Vec<Constraint<'ctx>> {
  match stmt {
    Bytecode::Jump(_, _) => vec![Bool::from_bool(context, true)],
    Bytecode::Branch(_, _, _, var) => {
      vec![s.get_slot(*var).to_condition(context), s.get_slot(*var).to_condition_neg(context)]
    }
    _ => panic!("Given non-branch instruction."),
  }
}

// Helper functions for cases of `eval_stmt`.
mod eval {
  use super::*;
  /// Used to evaluate an `Assign`.
  pub fn assign(dst: TempIndex, src: TempIndex, kind: AssignKind, s: &mut LocalState) {
    let src_val = match kind {
      AssignKind::Move => s.del_val(src),
      AssignKind::Copy | AssignKind::Store => s.get_val(src),
    };
    s.set(dst, src_val);
  }

  /// Used to evaluate a `Call`.
  // todo: handle `on_abort`
  pub fn operation<'ctx>(dsts: &[TempIndex], op: Operation, srcs: &[TempIndex], _on_abort: Option<&AbortAction>, s: &mut LocalState<'ctx>) {
    let res: Vec<Vec<ConstrainedValue<'ctx>>> = match op {
      Operation::Add => {
        let operands = s.get_constrained_operands(srcs);
        let res = {
          operands.into_iter().map(|x| {
            let (ops, constraint) = x;
            assert_eq!(ops.len(), 2, "Addition has two operands.");
            let res_val = ops[0].add(&ops[1]);
            ConstrainedValue::new(res_val, constraint)
          }).collect::<Vec<ConstrainedValue<'ctx>>>()
        };
        vec![res]
      }
      Operation::Lt => {
        let operands = s.get_constrained_operands(srcs);
        let res = operands.into_iter().map(|x| {
          let (ops, constraint) = x;
          assert_eq!(ops.len(), 2, "Arity mismatch. Lt has two operands, but given {}.", ops.len());
          let res_val = ops[0].lt(&ops[1]);
          ConstrainedValue::new(res_val, constraint)
        }).collect();
        vec![res]
      }
      _ => todo!(),
    };
    s.set_vars(dsts, res);
  }

  pub fn load(x: TempIndex, c: &Constant, s: &mut LocalState) {
    s.get_mut_slot(x).load_constant(c);
  }
}

// Evaluates a non-branch instruction. Panics if given a branch instruction.
fn eval_stmt(stmt: &Bytecode, s: &mut LocalState) {
  match stmt {
    Bytecode::Jump(_, _) => panic!(),
    Bytecode::Branch(_, _, _, _) => panic!(),
    Bytecode::Assign(_, dst, src, kind) => {
      eval::assign(*dst, *src, *kind, s)
    }
    Bytecode::Call(_, dsts, op, srcs, abort_action) => {
      eval::operation(dsts.as_slice(), op.clone(), srcs.as_slice(), abort_action.as_ref(), s)
    }
    Bytecode::Load(_, x, c) => eval::load(*x, c, s),
    _ => (),
  }
}

/// Updates state `s` by block `codes` and returns constraints to the children.
pub fn compute_block<'ctx>(codes: &[Bytecode], s: &mut LocalState<'ctx>, context: &'ctx Context) -> Vec<Constraint<'ctx>> {
  for stmt in codes {
    if is_jump_instruction(stmt) {
      return eval_jump(stmt, s, context);
    } else {
      eval_stmt(stmt, s);
    }
  }
  Vec::new()
}