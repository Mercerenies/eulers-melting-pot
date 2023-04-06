
// Optimizations applied to problem147.rs using summation formulas.

fn clamp(x: i64, a: i64, b: i64) -> i64 {
  i64::min(i64::max(x, a), b)
}

// Axis-aligned rectangles. Simplified summation.
fn count_aa_rectangles(height: i64, width: i64) -> i64 {
  (width * width + width) * (height * height + height) / 4
}

// Diagonal rectangles. I'm sure there *is* a way to get an explicit
// formula for this, but with all the i64::min's and the parity check
// on x + y, it seems like more trouble than it's worth, especially
// considering that this already runs in < 1 second.
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
      // The original formula in problem147.rs summed min(height - y,
      // width - x - w) where w ranged from 1 up to min(y, width - x).
      // We split this into two summations, using the point that the
      // min(height - y, width - x - w) crosses to the other side
      // (i.e. the w value that makes the two arguments to it equal)
      // as the midpoint.
      //
      // It's possible that this midpoint lies outside of the range
      // for w. So we clamp our values from 1 up to the original upper
      // limit. Do some minor index adjustments and use the triangle
      // number formula a few times, and you get rid of the summation.
      let upper = i64::min(y, width - x);
      let mid_bound = clamp(width + y - x - height + 1, 1, upper + 1);
      total += (height - y) * (mid_bound - 1);
      total += (width - x) * (upper - mid_bound + 1) - (upper * (upper + 1) - mid_bound * (mid_bound - 1)) / 2;
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
