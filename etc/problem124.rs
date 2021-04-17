
// Modified Sieve of Eratosthenes; stores a 0 for prime numbers and a
// prime factor for composite numbers.
pub fn sieve(upper: u32) -> Vec<u32> {
  let mut arr = Vec::with_capacity(upper as usize);
  arr.push(1);
  arr.push(1);
  for _ in 2..upper {
    arr.push(0);
  }
  for i in 2..upper {
    if arr[i as usize] == 0 {
      let mut j = i + i;
      while j < upper {
        arr[j as usize] = i;
        j += i;
      }
    }
  }
  arr
}

pub fn main() {
  let limit = 100001;
  let primes_arr = sieve(limit);

  // Identify distinct prime products
  let mut values_arr = Vec::with_capacity(limit as usize);
  values_arr.push(0); // The zero result is not really meaningful here
  values_arr.push(1); // The one result is a weird corner case, so hard-code it
  for i in 2..limit {
    let prime_factor = primes_arr[i as usize];
    if prime_factor == 0 {
      // Value is prime
      values_arr.push(i);
    } else {
      // Value has a nontrivial prime factor
      let prev = values_arr[(i / prime_factor) as usize];
      if prev % prime_factor == 0 {
        // Already included in prev, so don't multiply
        values_arr.push(prev);
      } else {
        // Need to multiply by the new distinct prime factor
        values_arr.push(prev * prime_factor);
      }
    }
  }

  let mut nums_arr = Vec::with_capacity(limit as usize);
  for i in 1..limit {
    nums_arr.push(i);
  }
  nums_arr.sort_unstable_by_key(|n| (values_arr[*n as usize], *n));
  println!("{}", nums_arr[9999]);

}
