
/// Naive solution, no attempt at optimization, for brute-forcing
/// `N(a, b, c)`.

// NOTE: Probably won't end up using this. I think I figured out the
// pattern over a nice bowl of ice cream. Check out 194_analysis.hs

use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Configuration {
  A, B,
}

/// The first index is either 0 or 1.
///
/// If the first index is zero, then the vertex being indexed is on
/// the top or bottom of the graph. The second and third indices are
/// the X and Y coordinates, where the Y coordinate is either 0 or 1
/// (0 = top, 1 = bottom), and the X coordinate is zero-indexed.
///
/// If the first index is one, then the vertex being indexed is in one
/// of the middle "thoroughfares" through the graph. The second index
/// is the X position (equivalently, the subgraph being traveled
/// through) starting at zero. The third is the Y position: either 0,
/// 1 or 2.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphIndex(u32, u32, u32);

#[derive(Debug, Clone)]
pub struct Graph<T> {
  adjacency: HashMap<T, Vec<T>>,
}

impl GraphIndex {
  pub fn top(x: u32) -> Self {
    Self(0, x, 0)
  }

  pub fn bottom(x: u32) -> Self {
    Self(0, x, 1)
  }

  pub fn middle(x: u32, y: u32) -> Self {
    assert!(y < 3);
    Self(1, x, y)
  }
}

impl<T: Hash> Graph<T> {
  pub fn new() -> Self {
    Graph {
      adjacency: HashMap::new(),
    }
  }
}

impl Graph<GraphIndex> {
  pub fn from_config(config: &[Configuration]) -> Self {
    ////
  }
}

impl<T: Hash> Default for Graph<T> {
  fn default() -> Self {
    Self::new()
  }
}
