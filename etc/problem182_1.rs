
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
  let p = 1009;
  let q = 3643;
  let limit = p * q;
  let phi = (p - 1) * (q - 1);
  let orders = get_orders(limit);
  // If m has order N, then for any integer k, e = k N + 1 will leave
  // m unconcealed. Mark all of the e values for which this is true
  // and keep a count.
  let mut unconcealed_per_e = vec![0i64; phi as usize];
  // 0 and 1 are not valid solutions, so don't use them.
  unconcealed_per_e[0] = i64::MAX;
  unconcealed_per_e[1] = i64::MAX;
  for order in orders.into_iter().skip(1) {
    if order == 1 {
      // Skip order 1; as we can never satisfy those values. Doing
      // this doesn't change the answer but does speed up the
      // computation.
      continue;
    }
    let mut i = order;
    while i + 1 < phi {
      unconcealed_per_e[(i + 1) as usize] += 1;
      i += order;
    }
  }

  // Disable all of the e values which are NOT coprime to phi.
  for e in 2..phi {
    if gcd(e, phi) != 1 {
      unconcealed_per_e[e as usize] = i64::MAX;
    }
  }

  // Find the minimum, and count all e's that match it.
  let min = *unconcealed_per_e.iter().min().unwrap();
  let mut sum = 0;
  for (e, unconcealed_count) in unconcealed_per_e.into_iter().enumerate() {
    if unconcealed_count == min {
      sum += e;
    }
  }
  println!("{}", sum);
}
