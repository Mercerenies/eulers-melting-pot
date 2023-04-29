
// We make a triangle of partial sums starting from x=0. So if our
// input triangle was this.
//
//    1
//   2 3
//  4 5 6
//
// Our partial sum triangle would be this.
//
//    1
//   2 5
//  4 9 15
//
// To calculate the sum of a contiguous sequence i..j in a row
// quickly, we take the sum 0..j and subtract the sum 0..(i-1). That
// means we can sum rows in O(1) time by precomputing everything.
//
// From there, just brute force everything. Y, X, and the triangle
// height range from 0 to 1000, so it's a cubic time algorithm. The
// actual "summing" part is O(1) since we made our partial sums up
// front in O(n). Takes 12 seconds to run.

const ROWS: usize = 1000;

#[derive(Debug)]
pub struct Triangle {
  data: Vec<Vec<i64>>,
}

impl Triangle {

  pub fn new(height: usize) -> Triangle {
    let mut data: Vec<Vec<i64>> = Vec::with_capacity(height);
    data.resize_with(height, Default::default);
    for i in 1..=height {
      data[i - 1].resize(i, 0);
    }
    Triangle { data }
  }

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

  pub fn put(&mut self, row: usize, column: usize, value: i64) {
    self.data[row][column] = value;
  }

  pub fn height(&self) -> usize {
    self.data.len()
  }

}

pub fn partial_sums(triangle: &Triangle) -> Triangle {
  let mut sum_triangle = Triangle::new(triangle.height());
  for y in 0..triangle.height() {
    let mut sum = 0;
    for x in 0..=y {
      sum += triangle.get(y, x);
      sum_triangle.put(y, x, sum);
    }
  }
  sum_triangle
}

fn main() {
  /*
  // The example triangle from the problem description
  let triangle = Triangle {
    data: vec!(vec!(15), vec!(-14, -7), vec!(20, -13, -5), vec!(-3, 8, 23, -26), vec!(1, -4, -5, -18, 5), vec!(-16, 31, 2, 9, 28, 3)),
  };
  */

  let triangle = Triangle::generate();
  let sum_triangle = partial_sums(&triangle);
  let mut best_sum = i64::MAX;

  for y in 0..ROWS {
    for x in 0..=y {
      let mut partial_sum = 0;
      for h in 0..(ROWS-y) {
        partial_sum += sum_triangle.get(y + h, x + h);
        if x > 0 {
          partial_sum -= sum_triangle.get(y + h, x - 1)
        }
        best_sum = i64::min(best_sum, partial_sum);
      }
    }
  }
  println!("{}", best_sum);
}
