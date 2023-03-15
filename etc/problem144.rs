
// Equation of ellipse: 4x^2 + y^2 = 100

#[derive(Debug, Clone, Copy, PartialEq)]
struct Beam {
  origin: Point,
  slope: f64,
  intercept: f64,
}

type Point = (f64, f64);

impl Beam {

  fn starting_position() -> Beam {
    Beam {
      origin: (0.0, 10.1),
      slope: -14.0714,
      intercept: 10.1,
    }
  }

  fn bounce_off_wall(&self) -> Beam {
    let new_origin = farthest_point_on_circle(self.origin, self.slope, self.intercept);
    let ellipse_normal_slope = new_origin.1 / (4.0 * new_origin.0);
    let new_slope = reflect_slope(ellipse_normal_slope, self.slope);
    let new_intercept = calculate_y_intercept(new_slope, new_origin);
    Beam {
      origin: new_origin,
      slope: new_slope,
      intercept: new_intercept,
    }
  }

  fn can_escape_circle(&self) -> bool {
    let (x, y) = self.origin;
    x >= -0.01 && x <= 0.01 && y > 0.0
  }

}

// Use this answer to reflect slopes: https://stackoverflow.com/a/17395912/2288659
fn reflect_slope(normal_slope: f64, m: f64) -> f64 {
  (normal_slope * normal_slope * m + 2.0 * normal_slope - m) / (1.0 + 2.0 * normal_slope * m - normal_slope * normal_slope)
}

// Given slope and a point, calculate Y intercept
fn calculate_y_intercept(slope: f64, point: Point) -> f64 {
  - slope * point.0 + point.1
}

fn distance_squared(p: Point, q: Point) -> f64 {
  let dx = p.0 - q.0;
  let dy = p.1 - q.1;
  return dx * dx + dy * dy;
}

// Find the collisions of the ellipse with the line y = ax + b
fn points_on_circle(a: f64, b: f64) -> [Point; 2] {
  let x0 = (- a * b + 2.0 * f64::sqrt(25.0 * a * a - b * b + 100.0)) / (4.0 + a * a);
  let y0 = a * x0 + b;
  let x1 = (- a * b - 2.0 * f64::sqrt(25.0 * a * a - b * b + 100.0)) / (4.0 + a * a);
  let y1 = a * x1 + b;
  [(x0, y0), (x1, y1)]
}

// Return the point further from origin that collides with the ellipse
// and y = ax + b
fn farthest_point_on_circle(origin: Point, a: f64, b: f64) -> Point {
  let [p, q] = points_on_circle(a, b);
  if distance_squared(origin, p) > distance_squared(origin, q) {
    p
  } else {
    q
  }
}

fn main() {
  let mut beam = Beam::starting_position();
  let mut bounces = 0;
  loop {
    beam = beam.bounce_off_wall();
    if beam.can_escape_circle() {
      break;
    }
    bounces += 1;
  }
  println!("{}", bounces);
}
