use bytecode::{
  stackless_bytecode::{
    Bytecode, Label, Constant,
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

// evaluate a jump or branch instruction, and return the path constraints of
// the out-edges
fn eval_jump<'ctx>(stmt: &Bytecode, s: &LocalState<'ctx>, context: &'ctx Context) -> Vec<Constraint<'ctx>> {
  match stmt {
    Bytecode::Jump(_, _) => vec![Bool::from_bool(context, true)],
    Bytecode::Branch(_, _, _, var) => {
      vec![s.get_slot(*var).to_condition(context), s.get_slot(*var).to_condition_neg(context)]
    }
    _ => panic!(),
  }
}

// evaluate a non-control-flow instruction
fn eval_stmt(stmt: &Bytecode, s: &mut LocalState) {
  match stmt {
    Bytecode::Jump(_, _) => panic!(),
    Bytecode::Branch(_, _, _, _) => panic!(),
    _ => todo!(),
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