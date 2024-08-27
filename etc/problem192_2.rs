
// Per
// https://en.wikipedia.org/wiki/Continued_fraction#Best_rational_approximations,
// we can calculate the candidates for the best possible rational by
// expanding the continued fraction for sqrt(N). The algorithm listed
// there is more sophisticated than what we do here. Here, we just
// generate all of the candidates and then check (using floating-point
// arithmetic) which one is the best.
//
// This very much does not work. It loses waaaay too much precision.

pub fn next_continued_fraction(value: f64) -> (i64, f64) {
  let int_part = value.floor();
  let frac_part = value - int_part;
  (int_part as i64, 1.0 / frac_part)
}

pub fn continued_fraction_terms(mut value: f64, max_denominator: i64) -> Vec<i64> {
  let mut result = Vec::new();
  let mut running_denom = 1;
  while running_denom <= max_denominator {
    let next = next_continued_fraction(value);
    result.push(next.0);
    running_denom = running_denom * next.0 + 1;
    value = next.1;
  }
  result
}

pub fn main() {
  println!("{:?}", continued_fraction_terms((2f64).sqrt(), 1_000_000_000_000));
}
