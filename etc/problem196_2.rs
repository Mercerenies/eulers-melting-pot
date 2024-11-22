
// Identical approach to problem196_1.scala. 23 seconds in Rust. Twice
// as fast as Scala, but not as fast as I was hoping.
//
// Never mind, 2 seconds with -O. Always turn on optimizations, kids.
// :)

use std::ops::{Add, Sub, Neg};
use std::collections::HashSet;

pub fn modulo(x: i64, y: i64) -> i64 {
  (x % y + y) % y
}

pub fn triangular_number(n: i64) -> i64 {
  n * (n + 1) / 2
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
  pub y: i64,
  pub x: i64,
}

#[derive(Debug, Clone)]
pub struct KnownPrimesSet {
  primes: HashSet<i64>,
  lower_bound: i64,
  upper_bound: i64,
}

impl Pos {
  pub const ADJACENCIES: [Pos; 8] = [
    Pos { y: -1, x: -1 }, Pos { y: -1, x: 0 }, Pos { y: -1, x: 1 },
    Pos { y:  0, x: -1 },                      Pos { y:  0, x: 1 },
    Pos { y:  1, x: -1 }, Pos { y:  1, x: 0 }, Pos { y:  1, x: 1 },
  ];

  pub fn adjacent(self) -> impl Iterator<Item = Self> {
    Self::ADJACENCIES.iter().map(move |a| self + *a)
  }

  pub fn adjacent_and_self(self) -> impl Iterator<Item = Self> {
    Self::ADJACENCIES.iter().map(move |a| self + *a).chain(std::iter::once(self))
  }
}

impl Add for Pos {
  type Output = Pos;

  fn add(self, other: Self) -> Self::Output {
    Pos {
      x: self.x + other.x,
      y: self.y + other.y,
    }
  }
}

impl Sub for Pos {
  type Output = Pos;

  fn sub(self, other: Self) -> Self::Output {
    self + (- other)
  }
}

impl Neg for Pos {
  type Output = Pos;

  fn neg(self) -> Self::Output {
    Pos {
      x: -self.x,
      y: -self.y,
    }
  }
}

impl KnownPrimesSet {
  pub fn new(primes: HashSet<i64>, lower_bound: i64, upper_bound: i64) -> Self {
    Self { primes, lower_bound, upper_bound }
  }

  pub fn is_prime(&self, n: i64) -> bool {
    if n < 2 {
      false
    } else if n < self.lower_bound || n > self.upper_bound {
      panic!("n is out of bounds: {} not in [{}, {}]", n, self.lower_bound, self.upper_bound)
    } else {
      self.primes.contains(&n)
    }
  }
}

pub fn partial_sieve(lower_bound: i64, upper_bound: i64) -> KnownPrimesSet {
  let s = (upper_bound as f64).sqrt() as i64 + 1;
  let mut low_primes = vec![true; s as usize];
  let mut high_primes = vec![true; (upper_bound - lower_bound) as usize];
  for i in 2..s {
    if low_primes[i as usize] {
      let mut j = i * i;
      while j < s {
        low_primes[j as usize] = false;
        j += i;
      }
      let mut j = lower_bound + modulo(- lower_bound, i);
      while j < upper_bound {
        high_primes[(j - lower_bound) as usize] = false;
        j += i;
      }
    }
  }
  let known_prime_values = high_primes.into_iter()
    .enumerate()
    .filter_map(|(i, is_prime)| if is_prime { Some(i as i64 + lower_bound) } else { None })
    .collect();
  KnownPrimesSet::new(known_prime_values, lower_bound, upper_bound)
}

fn row_width(row_number: i64) -> i64 {
  row_number + 1
}

fn get_number_for_triangle(pos: Pos) -> i64 {
  if pos.y < 0 || pos.x < 0 || pos.x >= row_width(pos.y) {
    0
  } else {
    let last_triangular_number = triangular_number(pos.y);
    last_triangular_number + pos.x + 1
  }
}

fn is_center_of_prime_triplet(primes: &KnownPrimesSet, pos: Pos) -> bool {
  primes.is_prime(get_number_for_triangle(pos)) &&
    pos.adjacent().filter(|p| primes.is_prime(get_number_for_triangle(*p))).count() >= 2
}

pub fn s(row: i64) -> i64 {
  let min_relevant = get_number_for_triangle(Pos { y: row - 2, x: 0 });
  let max_relevant = get_number_for_triangle(Pos { y: row + 2, x: row_width(row + 2) - 1 });
  let primes = partial_sieve(min_relevant, max_relevant);
  let min_bound_on_row = get_number_for_triangle(Pos { y: row, x: 0 });
  let max_bound_on_row = get_number_for_triangle(Pos { y: row, x: row_width(row) - 1 });
  let mut primes_in_any_triplet = HashSet::new();
  for y in (row - 1)..=(row + 1) {
    for x in 0..row_width(y) {
      let pos = Pos { y, x };
      if is_center_of_prime_triplet(&primes, pos) {
        for p in pos.adjacent_and_self() {
          let k = get_number_for_triangle(p);
          if (min_bound_on_row..=max_bound_on_row).contains(&k) && primes.is_prime(k) {
            primes_in_any_triplet.insert(k);
          }
        }
      }
    }
  }
  primes_in_any_triplet.into_iter().sum()
}

pub fn main() {
  println!("{}", s(5_678_026) + s(7_208_784));
}
