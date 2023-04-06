
// Naive solution to 147, no caching.

// Axis-aligned rectangles.
fn count_aa_rectangles(height: i64, width: i64) -> i64 {
  let mut total = 0;
  for i in 1..=height {
    for j in 1..=width {
      let placements = (height - i + 1) * (width - j + 1);
      total += placements;
    }
  }
  total
}

// Diagonal rectangles.
fn count_dia_rectangles(height: i64, width: i64) -> i64 {
  // Consider everything using "half coordinates", where every grid
  // position is the length of two Rust integers. Valid rectangle
  // sizes in this system are even integers, and valid starting
  // positions for a rectangle vertex must have (X+Y) even.
  let height = height * 2;
  let width = width * 2;
  let mut total = 0;
  for x in 0..width {
    for y in 0..height {
      // Must be a valid starting position
      if (x + y) % 2 == 1 {
        continue;
      }
      for w in 1..=i64::min(y, width - x) {
        let height_placements = i64::min(height - y, width - x - w);
        total += height_placements;
      }
    }
  }
  total
}

fn main() {
  let mut total = 0;
  for i in 1..=47 {
    for j in 1..=43 {
      total += count_aa_rectangles(i, j);
      total += count_dia_rectangles(i, j);
    }
  }
  println!("{}", total);
}
