
// Same as problem144.rs but with all of the intermediate types
// removed.

// Equation of ellipse: 4x^2 + y^2 = 100

fn main() {
  let mut origin_x = 0.0;
  let mut origin_y = 10.1;
  let mut slope = -14.0714;
  let mut intercept = 10.1;
  let mut bounces = 0;
  loop {
    let (new_x, new_y): (f64, f64);
    let px = (- slope * intercept + 2.0 * f64::sqrt(25.0 * slope * slope - intercept * intercept + 100.0)) / (4.0 + slope * slope);
    let py = slope * px + intercept;
    let qx = (- slope * intercept - 2.0 * f64::sqrt(25.0 * slope * slope - intercept * intercept + 100.0)) / (4.0 + slope * slope);
    let qy = slope * qx + intercept;
    let dx1 = px - origin_x;
    let dy1 = py - origin_y;
    let dx2 = qx - origin_x;
    let dy2 = qy - origin_y;
    if dx1 * dx1 + dy1 * dy1 > dx2 * dx2 + dy2 * dy2 {
      new_x = px;
      new_y = py;
    } else {
      new_x = qx;
      new_y = qy;
    }
    let ellipse_normal_slope = new_y / (4.0 * new_x);
    let new_slope = (ellipse_normal_slope * ellipse_normal_slope * slope + 2.0 * ellipse_normal_slope - slope) / (1.0 + 2.0 * ellipse_normal_slope * slope - ellipse_normal_slope * ellipse_normal_slope);
    origin_x = new_x;
    origin_y = new_y;
    slope = new_slope;
    intercept = - new_slope * new_x + new_y;
    if new_x >= -0.01 && new_x <= 0.01 && new_y > 0.0 {
      break;
    }
    bounces += 1;
  }
  println!("{}", bounces);
}

// Conventions:
//
// X, Y, and Z are function arguments, cannot be used for globals.
//
// M and O are Math and utils, respectively, so best not to override
// those.
//
// Japt seems to want to automagically use U, V, W for the first few
// lines of code, so best not to mess with those.
//
// GLOBALS:
// A = origin_x
// B = origin_y
// C = slope
// D = intercept
// E = bounces
//
// VARIABLES LOCAL TO OUR FUNCTION:
// (Still technically globals but we use them as locals)
// F = new_x
// G = new_y
// H = px
// I = py
// J = qx
// K = qy
// L = dx1
// N = dy1
// P = dx2
// Q = dy2
// R = ellipse_normal_slope
// S = new_slope
// T = our iteration function
