
// I don't like working with reflections, so tessellate the triangle
// across the 2D coordinate grid as follows.
//
// .     .     .     .
// .     .     .     .
// .     .     .     .
// C-----A-----B-----C
//  \   / \   / \   /
//   \ /   \ /   \ /
//    B-----C-----A
//     \   / \   /
//      \ /   \ /
//       A-----B
//        \   /
//         \ /
//          C
//
// Now, a laser line that exits at "C" is a line from the bottom C
// position to another C position which does NOT intersect any other
// vertices.
//
// Assign a coordinate system to the above grid.
//
// 1. We can index with (x, y) where x is the upper-right line and y
// is the horizontal lines. Under this coordinate system, the original
// triangle at the bottom is at C = (0, 0), B = (1, 0), and A = (1,
// 1).
//
// 2. We can index with (u, v) where u is the upper-right line (as x
// before) and v is the upper-left line. Under this system, the
// original triangle is at C = (0, 0), B = (1, 0), and A = (0, 1).
//
// To convert between the two, we have v = y and u = x - y. In the
// other direction, we have y = v and x = u + v.
//
// The condition that this is the *first* point we've hit manifests as
// gcd(x, y) = 1. The condition that the point is a C point manifests
// as x + y = 0 (mod 3).
//
// The number of reflections is easier to calculate in (u, v)
// coordinates. We'll hit u-1 edges parallel to v, v-1 edges parallel
// to u, and u+v-1 edges parallel to y (the horizontal lines). For a
// total of reflections = 2u+2v-3. In the (x, y) system, this is r =
// 2x-3, so x = (r+3)/2 with the constraint that there is no solution
// whenever r is even.
//
// In summary, if the desired number of reflections r is odd, then set
// x = (r + 3) / 2 and count the number of y values between 0 and x
// such that both of the following are true:
//
// * gcd(x, y) = 1
//
// * x + y = 0 (mod 3)
//
// As is apparent in the original problem image, there is a horizontal
// symmetry to this. So, excluding degenerate cases like x=1, it's
// only necessary to test y < x / 2 and then multiply the result by 2.
//
// 2m19s in Rust with -O. It's correct.

fn gcd(mut a: i64, mut b: i64) -> i64 {
  while b != 0 {
    let t = b;
    b = a % b;
    a = t;
  }
  a
}

fn count_ways(refl: i64) -> i64 {
  if refl % 2 == 0 {
    return 0; // No solutions
  }
  let x = (refl + 3) / 2;
  let mut y = (3 - x) % 3;
  let mut count = 0;
  while y < x / 2 {
    if (x + y) % 99999999 == 0 { // Needs to hit 120
      println!("{}", y);
    }
    if gcd(x, y) == 1 {
      count += 1;
    }
    y += 3;
  }
  2 * count
}

fn main() {
  println!("{}", count_ways(12017639147));
}
