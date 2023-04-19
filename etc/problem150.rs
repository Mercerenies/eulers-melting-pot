
// Caching solution not efficient enough

use std::collections::HashMap;

const ROWS: usize = 1000;

#[derive(Debug)]
struct Triangle {
  data: Vec<Vec<i64>>,
}

impl Triangle {

  pub fn generate() -> Triangle {
    let mut data: Vec<Vec<i64>> = Vec::with_capacity(ROWS);
    data.resize_with(ROWS, Default::default);

    let mut row = 0;
    let mut column = 0;
    let mut t = 0;

    for _ in 1..=500500 {
      // Set current position
      t = (615949 * t + 797807) % (1 << 20);
      data[row].push(t - (1 << 19));

      // Update row and column
      column += 1;
      if column > row {
        row += 1;
        column = 0;
      }
    }

    Triangle { data }
  }

  pub fn get(&self, row: usize, column: usize) -> i64 {
    self.data[row][column]
  }

  pub fn get_sum(&self, start_row: usize, start_column: usize, side_length: usize) -> i64 {
    let mut sum = 0;
    for i in 0..side_length {
      for j in 0..=i {
        sum += self.get(start_row + i, start_column + j);
      }
    }
    sum
  }

  pub fn row_count(&self) -> usize {
    self.data.len()
  }

  pub fn full_candidate(&self) -> Candidate {
    Candidate { row: 0, column: 0, length: self.row_count() }
  }

  pub fn sum_of_all(&self) -> i64 {
    self.get_sum(0, 0, self.row_count())
  }

}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct Candidate {
  pub row: usize,
  pub column: usize,
  pub length: usize,
}

impl Candidate {

  pub fn bottom(self) -> Candidate {
    Candidate { row: self.row, column: self.column, length: self.length - 1 }
  }

  pub fn left(self) -> Candidate {
    Candidate { row: self.row + 1, column: self.column + 1, length: self.length - 1 }
  }

  pub fn right(self) -> Candidate {
    Candidate { row: self.row + 1, column: self.column, length: self.length - 1 }
  }

  pub fn apply(self, op: Op) -> Candidate {
    match op {
      Op::Bottom => self.bottom(),
      Op::Left => self.left(),
      Op::Right => self.right(),
    }
  }

}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Op { Bottom, Left, Right }

#[derive(Debug, Default)]
struct Cache {
  data: HashMap<Candidate, i64>,
}

impl Cache {

  pub fn new() -> Cache {
    Cache::default()
  }

  pub fn compute_best_sum(&mut self, triangle: &Triangle, current_candidate: Candidate, current_sum: i64) -> i64 {
    if current_candidate.length == 1 {
      // Smallest possible triangle; base case
      current_sum
    } else if let Some(result) = self.data.get(&current_candidate) {
      *result
    } else {
      // Try shrinking in each of the three directions
      let mut bottom_sum = current_sum;
      let mut left_sum = current_sum;
      let mut right_sum = current_sum;
      for i in 0..current_candidate.length {
        bottom_sum -= triangle.get(current_candidate.row + current_candidate.length - 1, current_candidate.column + i);
        left_sum -= triangle.get(current_candidate.row + i, current_candidate.column);
        right_sum -= triangle.get(current_candidate.row + i, current_candidate.column + i);
      }
      let result = i64::min(
        i64::min(
          current_sum,
          self.compute_best_sum(triangle, current_candidate.bottom(), bottom_sum),
        ),
        i64::min(
          self.compute_best_sum(triangle, current_candidate.left(), left_sum),
          self.compute_best_sum(triangle, current_candidate.right(), right_sum),
        ),
      );
      self.data.insert(current_candidate, result);
      result
    }
  }

}

fn main() {

/*
  // The example triangle from the problem description
  let triangle = Triangle {
    data: vec!(vec!(15), vec!(-14, -7), vec!(20, -13, -5), vec!(-3, 8, 23, -26), vec!(1, -4, -5, -18, 5), vec!(-16, 31, 2, 9, 28, 3)),
  };
*/

  let triangle = Triangle::generate();
  let mut cache = Cache::new();

  let candidate = Candidate { row: 0, column: 0, length: 200 };
  let sum = triangle.get_sum(0, 0, 200);

  //let result = cache.compute_best_sum(&triangle, triangle.full_candidate(), triangle.sum_of_all());
  let result = cache.compute_best_sum(&triangle, candidate, sum);
  println!("{}", result);

/*
  let triangle = Triangle::generate();
  let mut best_solution = 0;
  for row in 0..ROWS {
    println!("{}", row);
    for column in 0..=row {
      for side_length in 1..=(ROWS-row) {
        best_solution = i64::max(best_solution, triangle.get_sum(row, column, side_length));
      }
    }
  }
  println!("{}", best_solution);
*/
}
