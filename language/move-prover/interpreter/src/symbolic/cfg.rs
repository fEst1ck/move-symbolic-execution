use bytecode::{
  stackless_control_flow_graph::{
    StacklessControlFlowGraph,
    BlockId,
  },
};
use crate::concrete::{
  local_state::{LocalState},
};
use petgraph::graph::{Graph, NodeIndex};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

type Map<K, V> = BTreeMap<K, V>;
type Set<V> = BTreeSet<V>;

pub fn stackless_cfg_to_graph(cfg: &StacklessControlFlowGraph) -> Graph<BlockId, ()> {
  let mut g = Graph::<BlockId, ()>::new();
  let mut blockid_to_node_index = Map::<BlockId, NodeIndex>::new();
  let visited: Set<BlockId> = Set::new();
  let mut frontier: VecDeque<BlockId> = VecDeque::new();
  frontier.push_back(cfg.entry_block());
  while !fronghtier.is_empty() {
    let now_explore = frontier.pop_front().unwrap();
    for to_explore in cfg.successors(now_explore) {
      if !visited.contains(&now_explore) {
        g.add_node(now_explore);
        // g.add_edge(a: NodeIndex<Ix>, b: NodeIndex<Ix>, weight: E)
      }
    }
  }
  todo!()
}

