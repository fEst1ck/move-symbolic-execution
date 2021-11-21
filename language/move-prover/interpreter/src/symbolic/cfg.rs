use bytecode::{
  stackless_control_flow_graph::{
    StacklessControlFlowGraph,
    BlockId,
  },
  stackless_bytecode::{Bytecode},
};
use crate::symbolic::{
  local_state::{LocalState},
  value::{Constraint},
  sym_exec,
};
use z3::{Context, ast::{Bool}};
use petgraph::{
  graph::{Graph, NodeIndex, EdgeReference},
  visit::EdgeRef,
};
use petgraph::Direction;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ops::{Index, IndexMut};

type Map<K, V> = BTreeMap<K, V>;
type Set<V> = BTreeSet<V>;

// `constraint` is the path constraint under which the node is reachable
pub struct Node<'ctx> {
  block: BlockId,
  state: Option<LocalState<'ctx>>,
  constraint: Option<Constraint<'ctx>>
}

impl<'a> Node<'a> {
  pub fn done(&self) -> bool {
    self.state.is_some() && self.constraint.is_some()
  }
}

// `constraint` is the condition of the jump
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
  pub fn new(cfg: StacklessControlFlowGraph, codes: &'a [Bytecode]) -> Self {
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
      let now_explore: BlockId = frontier.pop_front().unwrap();
      visited.insert(now_explore);
      for to_explore in cfg.successors(now_explore) {
        if !visited.contains(&now_explore) {
          frontier.push_back(*to_explore);
          graph.add_edge(*block_id_to_node_index.get(&now_explore).unwrap(),
                       *block_id_to_node_index.get(&to_explore).unwrap(),
                       Edge { constraint: None });
        }
      }
    }
    Self { codes, cfg , graph, block_id_to_node_index }
  }

  // (u; q) ~p~> v
  // returns q and p
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
      constraints_ref.push(c)
    }
    Some(Bool::or(ctx, &constraints_ref))
  }

  fn compute_init_state(&self, index: NodeIndex) -> LocalState {
    todo!()
  }

  // check if the computations before `index` are done
  fn check_prereq(&self, index: NodeIndex) -> bool {
    for in_edge in self.graph.edges_directed(index, Direction::Incoming) {
      if !self.graph[in_edge.source()].done() { return false }
      if !in_edge.weight().done() { return false }
    }
    true
  }

  // do the compuation on `index`
  fn compute_node(&mut self, index: NodeIndex, ctx: &'ctx Context) -> bool {
    if self.check_prereq(index) {
      self.index_mut(index).constraint = self.compute_constraint(index, ctx);
      let mut state = self.compute_init_state(index);
      
      todo!()
    } else {
      false
    }
  }
}
