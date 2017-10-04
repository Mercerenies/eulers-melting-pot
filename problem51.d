
struct CountPair {
  int count;
  long smallest;
}

bool[long] cache;

bool is_prime_impl(long n) {
  if (n < 2)
    return false;
  for (long i = 2; i <= n / 2; i++) {
    if (n % i == 0)
      return false;
  }
  return true;
}

bool is_prime(long n) {
  bool* check = (n in cache);
  if (check is null)
    cache[n] = is_prime_impl(n);
  return cache[n];
}

int digits(long n) {
  import std.math, std.conv;
  return to!int(log10(n) + 1);
}

CountPair count_primes(long n, long mask) {
  int j = 0;
  n -= mask;
  long smallest = -1;
  for (int i = 0; i < 10; i++) {
    if (is_prime(n)) {
      j += 1;
      if (smallest == -1)
        smallest = n;
    }
    n += mask;
  }
  return CountPair(j, smallest);
}

long generate_mask_impl(long n, long place, long acc) {
  if (n == 0)
    return acc;
  if (n % 10 == 1)
    acc += place;
  return generate_mask_impl(n / 10, place * 10, acc);
}

long generate_mask(long n) {
  return generate_mask_impl(n, 1, 0);
}

void main() {
  import std.stdio, std.conv;
  long i = 56003;
  while (i++) {
    long mask = generate_mask(i);
    CountPair pair = count_primes(i, mask);
    if ((mask > 0) && (pair.count >= 8) && (digits(pair.smallest) == digits(i))) {
      writeln(to!string(pair.smallest));
      break;
    }
  }
}
