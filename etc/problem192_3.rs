
// Using the recurrence described here
// (https://math.stackexchange.com/a/4425617/84460), generate all
// convergents until the denominator hits our limit, then test those
// convergents. I'm going to just test convergents for now, and I'll
// add semiconvergents if it needs them, but Wikipedia seems to have
// contrary information on whether or not the semiconvergents are
// necessary.

#[derive(Debug, Clone)]
struct FractionState {
  a: i64,
  r: i64,
  s: i64,
}

pub fn integer_sqrt(value: i64) -> i64 {
  // Could do this with iteration or some sort of bisection technique,
  // but floating-point should be good enough. :)
  (value as f64).sqrt() as i64
}

fn next_continued_fraction(n: i64, a0: i64, prev: &FractionState) -> FractionState {
  let r = prev.a * prev.s - prev.r;
  let s = (n - r * r) / prev.s;
  let a = (r + a0) / s;
  FractionState { a, r, s }
}

pub fn continued_frac_of_sqrt(n: i64, max_denom: i64) -> Vec<i64> {
  let a0 = integer_sqrt(n);
  let mut result = vec![a0];
  let mut state = FractionState { a: a0, r: 0, s: 1 };
  while continued_frac(&result).1 <= max_denom {
    //println!("{:?}", continued_frac(&result));
    state = next_continued_fraction(n, a0, &state);
    result.push(state.a);
  }
  result
}

pub fn continued_frac(cont_frac: &[i64]) -> (i64, i64) {
  assert!(!cont_frac.is_empty());
  if cont_frac.len() == 1 {
    (cont_frac[0], 1)
  } else {
    let n = cont_frac[0];
    let (a, b) = continued_frac(&cont_frac[1..]);
    (n * a + b, a)
  }
}

pub fn find_best_semiconvergent(mut cont_frac: Vec<i64>, max_denom: i64) -> (i64, i64) {
  while !cont_frac.is_empty() {
    let original_value = cont_frac[cont_frac.len() - 1];
    while 2 * cont_frac[cont_frac.len() - 1] > original_value {
      let (a, b) = continued_frac(&cont_frac);
      if b <= max_denom {
        return (a, b);
      }
      let last = cont_frac.len() - 1;
      cont_frac[last] -= 1;
    }
    cont_frac.pop();
  }
  panic!("no semiconvergent found");
}

pub fn main() {
  let limit: i64 = 1_000_000_000_000;

  let mut sum = 0;
  let mut next_integer_sqrt = 2;
  for n in 2..=100_000 {
    if next_integer_sqrt * next_integer_sqrt == n {
      next_integer_sqrt += 1;
      continue;
    }
    let (_, denom) = find_best_semiconvergent(continued_frac_of_sqrt(n, limit), limit);
    sum += denom;
  }

  println!("{}", sum);
  //println!("sqrt(13) = {:?}", find_best_semiconvergent(continued_frac_of_sqrt(13, 30), 30));
  //println!("sqrt(13) = {:?}", continued_frac(&continued_frac_of_sqrt(13, 30)));
  //println!("sqrt(2) = {:?}", continued_frac_of_sqrt(2, limit));
  //println!("sqrt(3) = {:?}", continued_frac_of_sqrt(3, limit));
  //println!("sqrt(5) = {:?}", continued_frac_of_sqrt(5, limit));
  //println!("sqrt(6) = {:?}", continued_frac_of_sqrt(6, limit));
  //println!("sqrt(10) = {:?}", continued_frac_of_sqrt(10, limit));
}
