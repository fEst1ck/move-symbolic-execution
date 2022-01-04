use move_stackless_bytecode::{
  stackless_control_flow_graph::{
    StacklessControlFlowGraph,
    BlockId,
    BlockContent,
  },
  stackless_bytecode::{Bytecode, Operation, Constant},
};
use crate::symbolic::{
  local_state::{LocalState},
  value::{Constraint},
  sym_exec::{compute_block},
};
use z3::{Context, ast::{Bool}};
use petgraph::{
  graph::{Graph, NodeIndex, EdgeReference},
  visit::{EdgeRef, IntoNeighborsDirected, IntoNodeIdentifiers},
  algo::{toposort},
};
use petgraph::Direction;
use petgraph::dot::{Dot};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ops::{Index, IndexMut};
use std::fmt;

type Map<K, V> = BTreeMap<K, V>;
type Set<V> = BTreeSet<V>;

// `constraint` is the path constraint under which the node is reachable
#[derive(Clone, Eq, PartialEq)]
pub struct Node<'ctx> {
  block: BlockId,
  state: Option<LocalState<'ctx>>,
  /// Accumulated path constraint.
  constraint: Option<Constraint<'ctx>>
}

impl<'ctx> fmt::Debug for Node<'ctx> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
      fmt.write_fmt(format_args!(
        "Block {}
        Accumulated path constraint: {:?}
        ----------
        {:?}", self.block, self.constraint, self.state))
  }
}

impl<'a> Node<'a> {
  pub fn done(&self) -> bool {
    self.state.is_some() && self.constraint.is_some()
  }
}

// `constraint` is the condition of the jump
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Edge<'ctx>{
  constraint: Option<Constraint<'ctx>>,
}

impl<'ctx> Edge<'ctx> {
  pub fn done(&self) -> bool {
    match self {
      Edge { constraint: Some(_) } => true,
      _ => false,
    }
  }
}

pub struct ControlFlowStateGraph<'a, 'ctx> {
  codes: &'a [Bytecode],
  cfg: StacklessControlFlowGraph,
  graph: Graph<Node<'ctx>, Edge<'ctx>>,
  block_id_to_node_index: Map<BlockId, NodeIndex>,
  initial_state: LocalState<'ctx>,
}

impl<'a, 'ctx> Index<NodeIndex> for ControlFlowStateGraph<'a, 'ctx> {
  type Output = Node<'ctx>;
  fn index(&self, index: NodeIndex) -> &Self::Output {
    self.graph.index(index)
  }
}

impl<'a, 'ctx> IndexMut<NodeIndex> for ControlFlowStateGraph<'a, 'ctx> {
  fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
    self.graph.index_mut(index)
  }
}

impl<'a, 'ctx> ControlFlowStateGraph<'a, 'ctx> {
  pub fn new(cfg: StacklessControlFlowGraph, codes: &'a [Bytecode], initial_state: LocalState<'ctx>) -> Self {
    let mut graph: Graph<Node, Edge> = Graph::new();
    let mut block_id_to_node_index: Map<BlockId, NodeIndex> = Map::new();
    // add all blocks into `res`
    for block in cfg.blocks() {
      let block_index = graph.add_node(Node { block: block, state: None, constraint: None });
      block_id_to_node_index.insert(block, block_index);
    }
    // add all edges to `res`
    let mut visited: Set<BlockId> = Set::new();
    let mut frontier: VecDeque<BlockId> = VecDeque::new();
    frontier.push_back(cfg.entry_block());
    while !frontier.is_empty() {
      let mut now_explore: BlockId = frontier.pop_front().unwrap();
      // handle the dummy exit
      if !block_id_to_node_index.contains_key(&now_explore) {
        now_explore = 1;
      }
      if visited.contains(&now_explore) {
        continue;
      }

      visited.insert(now_explore);
      for mut to_explore in cfg.successors(now_explore) {
        // handle the dummy exit
        if !block_id_to_node_index.contains_key(&to_explore) {
          to_explore = &1;
        }
        if !visited.contains(&to_explore) {
          frontier.push_back(*to_explore);
          println!("adding edge {:?} {:?}", now_explore, to_explore);
          graph.add_edge(*block_id_to_node_index.get(&now_explore).unwrap(),
                       *block_id_to_node_index.get(&to_explore).unwrap(),
                       Edge { constraint: None });
        }
      }
    }

    Self { codes, cfg , graph, block_id_to_node_index, initial_state }
  }

  pub fn get_graph(&self) -> &Graph<Node<'ctx>, Edge<'ctx>> {
    &self.graph
  }

  // Computes the accumulated constraints along en edge.
  // If `edge` is from node `u`, then
  // produces `u.constraint && p.constraint`.
  fn compute_path_constraint(&self, edge: EdgeReference<Edge<'ctx>>, ctx: &'ctx Context) -> Option<Constraint<'ctx>> {
    let parent = self.index(edge.source());
    if !edge.weight().done() || !parent.done() {
      return None
    } else {
      let new_constraint = edge.weight().constraint.as_ref().unwrap();
      let accumulated_constraint = parent.constraint.as_ref().unwrap();
      Some(Bool::and(ctx, &vec![accumulated_constraint, new_constraint]))
    }
  }

  // Produces the state after the jump.
  // Panics if the computation on parent isn't done.
  fn compute_path_state(&self, edge: EdgeReference<Edge<'ctx>>, ctx: &'ctx Context) -> LocalState<'ctx> {
    let parent = self.index(edge.source());
    let mut init_state: LocalState<'ctx> = parent.state.clone().unwrap();
    init_state.add_constraint(edge.weight().constraint.as_ref().unwrap(), ctx);
    init_state
  }

  /// Produces the accumulated constraint of node `index`.
  /// 
  /// If node `index` has parents and constraints to it (s, p) ..., then
  /// the accumulated constraint of `index` is (or (and s p) ...).
  fn compute_constraint(&self, index: NodeIndex, ctx: &'ctx Context) -> Option<Constraint<'ctx>> {
    let mut constraints: Vec<Bool<'ctx>> = Vec::new();
    for in_edge in self.graph.edges_directed(index, Direction::Incoming) {
      let constraint = self.compute_path_constraint(in_edge, ctx);
      if constraint.is_some() {
        constraints.push(constraint.unwrap());
      } else {
        return None;
      }
    }
    let mut constraints_ref: Vec<&Bool<'ctx>> = Vec::with_capacity(constraints.len());
    for c in &constraints {
      constraints_ref.push(c);
    }
    Some(Bool::or(ctx, &constraints_ref))
  }

  /// Produces the initial state of node `index`.
  fn compute_init_state(&self, index: NodeIndex, ctx: &'ctx Context) -> LocalState<'ctx> {
    let states: Vec<LocalState> = self.graph.edges_directed(index, Direction::Incoming)
                                            .map(|x| self.compute_path_state(x, ctx)).collect();
    if states.len() == 0 {
      self.initial_state.clone()
    } else if states.len() == 1 {
      states[0].clone()
    } else {
      LocalState::merge(states[0].clone(), states[1].clone())
    }
  }

  /// Checks if the computations before `index` are done.
  fn check_prereq(&self, index: NodeIndex) -> bool {
    for in_edge in self.graph.edges_directed(index, Direction::Incoming) {
      if !self.graph[in_edge.source()].done() { return false }
      if !in_edge.weight().done() { return false }
    }
    true
  }

  /// Gets the code block of node `index`.
  pub fn get_codes(&self, index: NodeIndex) -> &[Bytecode] {
    let block_id =  self.index(index).block;
    match self.cfg.content(block_id) {
      BlockContent::Dummy => &[],
      BlockContent::Basic{ lower, upper } => &self.codes[*lower as usize..(*upper + 1) as usize],
    }
  }

  /// Does the compuation on `index`.
  /// 
  /// This will set the node weight to the final state of the block
  /// and the weights of the outgoing edges to their corresponding constraints.
  fn compute_node(&mut self, index: NodeIndex, ctx: &'ctx Context) -> bool {
    if self.check_prereq(index) {
      self.index_mut(index).constraint = self.compute_constraint(index, ctx);
      let mut state = self.compute_init_state(index, ctx);
      let constraints = compute_block(self.get_codes(index), &mut state, ctx);
      self.index_mut(index).state = Some(state);
      // set out edges
      let out_edges: Vec<(NodeIndex, NodeIndex)> = self.graph.edges(index).map(|e| (e.source(), e.target())).collect();
      // treats the special case when block is dummy
      if !out_edges.is_empty() && constraints.is_empty() {
        assert_eq!(out_edges.len(), 1);
        let (s, t) = &out_edges[0];
        self.graph.update_edge(*s, *t, Edge{ constraint: Some(Bool::from_bool(ctx, true)) });
        return true;
      }
      assert_eq!(constraints.len(), out_edges.len());
      for ((source, target), constraint) in out_edges.into_iter().zip(constraints.into_iter()) {
        self.graph.update_edge(source, target, Edge{ constraint: Some(constraint) });
      }
      true
    } else {
      false
    }
  }

  pub fn main(&mut self, ctx: &'ctx Context) {
    match toposort(&self.graph, None) {
      Ok(nodes) => {
        for node in nodes {
          assert!(self.compute_node(node, ctx), "Wrong order of computation!");
        }
      }
      Err(_) => todo!(),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use Bytecode::*;
  use bytecode::stackless_bytecode::{AssignKind, AttrId, Label};
  use crate::symbolic::{
    value::{Type, ConstrainedValue, Value},
  };
  use z3::{Config, Context};
  use z3::{ast::{Bool, Int}};

  use petgraph::Graph;
  use petgraph::dot::{Dot};

  #[test]
  fn relu() {
    let dummy = AttrId::new(0);
    let codes = vec![
      Load(dummy, 2, Constant::U64(0)),
      Assign(dummy, 3, 0, AssignKind::Copy),
      Call(dummy, vec![4], Operation::Lt, vec![2, 3], None),
      Branch(dummy, Label::new(0), Label::new(1), 4),
      Label(dummy, Label::new(1)),
      Jump(dummy, Label::new(2)),
      Label(dummy, Label::new(0)),
      Assign(dummy, 5, 0, AssignKind::Copy),
      Assign(dummy, 1, 5, AssignKind::Store),
      Jump(dummy, Label::new(3)),
      Label(dummy, Label::new(2)),
      Load(dummy, 6, Constant::U64(0)),
      Assign(dummy, 1, 6, AssignKind::Copy),
      Jump(dummy, Label::new(3)),
      Label(dummy, Label::new(3)),
      Assign(dummy, 7, 1, AssignKind::Move),
    ];
    let cfg = StacklessControlFlowGraph::new_forward(codes.as_slice());
    cfg.display();
    let types = vec![
      Type::mk_num(),
      Type::mk_num(),
      Type::mk_num(),
      Type::mk_num(),
      Type::mk_bool(),
      Type::mk_num(),
      Type::mk_num(),
      Type::mk_num(),
      ];
    let config = Config::new();
    let ctx = Context::new(&config);
    let mut mem = LocalState::from_types(types.as_slice(), &ctx);
    mem.get_mut_slot(0).set(vec![ConstrainedValue::from_value(Value::Int(Int::fresh_const(&ctx, "x")), &ctx)]);
    let mut cfsg = ControlFlowStateGraph::new(cfg, codes.as_slice(), mem);
    cfsg.main(&ctx);
    println!("{:?}", Dot::new(cfsg.get_graph()));
  }
}