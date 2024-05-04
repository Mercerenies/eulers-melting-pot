
use std::collections::HashSet;

fn gcd(mut a: i64, mut b: i64) -> i64 {
  while b != 0 {
    let t = b;
    b = a % b;
    a = t;
  }
  a
}

// Calculate the multiplicative orders for all elements 0 <= m < n.
// The resulting vector is of length n and is indexed by the value.
// Elements in [0, n) which are NOT coprime to n will have value zero
// in the resulting array.
//
// Assumes n is squarefree (equivalently, if k != 0 then no power of k
// is zero modulo n). If this precondition is false, we might either
// hang or panic.
fn get_orders(n: i64) -> Vec<i64> {
  let mut result = vec![0; n as usize];
  for m in 1..n {
    if result[m as usize] != 0 {
      continue // We already calculated this as part of another value.
    }
    let mut curr: i64 = (m * m) % n;
    let mut i: i64 = 2;
    while curr != m {
      if curr == 0 {
        panic!("n was not square-free!");
      }
      if result[curr as usize] != 0 {
        // We know m ^ i == curr and we know the order of curr, so the
        // order of m is i * that order. Then the fixed point for m is
        // one more than that number.
        i = i * result[curr as usize] + 1;
        break;
      }
      curr = (curr * m) % n;
      i += 1;
    }
    // We can now calculate the order of all powers of m and store
    // them in the array. Go until we hit something we've already
    // seen.
    let order = i - 1;
    if order == 1 {
      result[m as usize] = 1;
    } else {
      let mut tmp = m;
      for j in 1..order {
        if result[tmp as usize] != 0 {
          break // We've already seen this value, so stop here.
        }
        result[tmp as usize] = order / gcd(order, j);
        tmp = (tmp * m) % n;
      }
    }
  }
  result
}

fn main() {
  let p = 19; // 1009;
  let q = 37; // 3643;
  let limit = p * q;
  let phi = (p - 1) * (q - 1);
  let orders = get_orders(limit);
  let orders: HashSet<_> = orders.into_iter().skip(1).collect();
  // For each order N in the orders set and any integer k, we want to
  // avoid e = k N + 1, since that will produce at least one
  // nontrivial unconcealed message. We ignore anything of order 1,
  // since any message with multiplicative order 1 will ALWAYS be an
  // unconcealed message, regardless of our choice of e.
  let mut desired_e_values_bitmask = vec![true; phi as usize];
  desired_e_values_bitmask[0] = false;
  desired_e_values_bitmask[1] = false;
  for order in orders {
    if order == 1 {
      continue; // Skip order 1; as we can never satisfy those values
    }
    let mut i = order;
    while i + 1 < phi {
      desired_e_values_bitmask[(i + 1) as usize] = false;
      i += order;
    }
  }
  let mut sum = 0;
  for (e, mask) in desired_e_values_bitmask.into_iter().enumerate() {
    if gcd(e as i64, phi) == 1 && mask {
      sum += e;
    }
  }
  println!("{}", sum);
}
