// -*- Rust -*- (Syntax is pretty close)

const ROW_COUNT = 8;

// Naive power function (good enough for our use case)
fn ipow(a: int, b: int) -> int {
  let mut result = 1;
  for _ in 0..b {
    result *= a;
  }
  return result;
}

fn make_vec(length: int) -> vec<int> {
  let mut v = vec<int>[];
  for _ in 0..length {
    v.push(0);
  }
  return v;
}

fn sum(v: vec<int>) -> int {
  let mut s: int = 0;
  for i in v {
    s += i;
  }
  return s;
}

fn main() {
  let mut downward_colors = vec[1, 1, 1];
  for n in 2..=ROW_COUNT {
    let mut new_colors = make_vec(ipow(3, n));
    for i in 0..ipow(3, n - 1) {
      for j in 0..ipow(3, n) {
        let mut product = 1;
        let mut c = i;
        let mut d = j;
        for _ in 1..n {
          let mut valid_colors = vec[1, 1, 1];
          valid_colors[c % 3] = 0;
          valid_colors[d % 3] = 0;
          valid_colors[(d / 3) % 3] = 0;
          c /= 3;
          d /= 3;
          product *= (valid_colors[0] + valid_colors[1] + valid_colors[2]);
        }
        new_colors[j] += downward_colors[i] * product;
      }
    }
    downward_colors = new_colors;
  }

  println(sum(downward_colors) as str);
}
