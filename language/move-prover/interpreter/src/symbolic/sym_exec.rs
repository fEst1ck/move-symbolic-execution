use bytecode::{
  function_target::FunctionTarget,
  stackless_bytecode::{
    Bytecode, Label, Constant,
  },
  stackless_control_flow_graph::{
    StacklessControlFlowGraph,
    BlockContent,
  },
};
use std::collections::{BTreeMap, BTreeSet};
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
use z3::{Context};

type Set<T> = BTreeSet<T>;
// type ConstrainedLabel = (Label, Constraints);

// enum EvalEffect {
//   None,
//   Goto(Label),
//   Branch(ConstrainedLabel, ConstrainedLabel)
// }

// mod stmt {
//   // evaluate a single bytecode
//   pub fn eval(stmt: &Bytecode, s: &mut LocalState)
//   -> EvalEffect {
//     match bytecode {
//       Bytecode::Load(_, dst, constant) => handle_load(*dst, constant, s),
//       _ => todo!(),
//     }
//   }

//   fn handle_load(dst: TempIndex, constant: &Constant, local_state: &mut LocalState) {
//     let val = match constant {
//         Constant::Bool(v) => TypedValue::mk_bool(v, local_state.context),
//         _ => todo!(),
//     };
//     local_state.put_value_override(dst, val);
//   }
// }

// Collect variables appearing on the LHS of the block.
// fn used_vars(block: &[Bytecode]) -> Set<TempIndex> {
//   fn exract_var(stmt: &Bytecode) -> Vec<TempIndex> {
//     match stmt {
//       Bytecode::Assign(_, dst, _, _) => vec![*dst],
//       Bytecode::Call(_, dsts, _, _, _) => *dsts,
//       Bytecode::Load(_, dst, _) => vec![*dst],
//       _ => vec![],
//     }
//   }
//   let res = Set::new();
//   for stmt in block.iter() {
//     for v in exract_var(stmt).iter() {
//       res.insert(v);
//     }
//   }
//   res
// }

// Evaluate a block of bytecodes, of which only the last can be
// a jump or branch.
// pub fn eval_block(codes: &[Bytecode], p: Constraints, s: mut LocalState)
// -> (LocalState, EvalEffect) {
//   for (i, stmt) in codes.iter().enumerate() {
//     let res = eval_stmt(stmt, p, &mut s);
//     match res {
//       EvalEffect::None => (),
//       _ => {
//         assert_eq!(i, codes.len() - 1);
//         return (s, res);
//       }
//     }
//   }
//   (s, EvalEffect::None)
// }

pub fn compute_block<'ctx>(codes: &[Bytecode], s: &mut LocalState<'ctx>, p: &Constraint<'ctx>) -> Vec<Constraint<'ctx>> {
  todo!()
}