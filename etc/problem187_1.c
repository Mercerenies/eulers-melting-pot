
#include <stdio.h>

// Trying problem187.c with Miller Rabin to see if it's viable without
// the whole array.
//
// It works, but it takes 1m15s. That's almost 20 times as slow as the
// Sieve of Eratosthenes method.

typedef int _bool;

#define LIMIT 100000000

#define TRUE 1
#define FALSE 0

unsigned long long modulo(unsigned long long a, unsigned long long b) {
  return (a % b + b) % b;
}

unsigned long long pow_mod(unsigned long long a, unsigned long long b, unsigned long long x) {
  if (b == 0ll) {
    return 1ll;
  } else if (b % 2 == 0) {
    unsigned long long intermediate = pow_mod(a, b / 2, x);
    return modulo(intermediate * intermediate, x);
  } else {
    return modulo(pow_mod(a, b - 1, x) * a, x);
  }
}

_bool do_miller_rabin_test(long long n, long long d, long long r, long long a) {
  long long x = pow_mod(a, d, n);
  if ((x == 1) || (x == n - 1))
      return TRUE;
  for (long long i = 0ll; i < r - 1; i++) {
    x = modulo(x * x, n);
    if (x == n - 1)
      return TRUE;
  }
  return FALSE;
}

_bool is_prime(long long n) {
  if (n < 2)
    return FALSE;
  if ((n == 2) || (n == 3) || (n == 5) || (n == 7))
    return TRUE;
  if ((n % 2 == 0) || (n % 3 == 0) || (n % 5 == 0) || (n % 7 == 0))
    return FALSE;

  long long d = n - 1ll;
  long long r = 0ll;
  while (d % 2 == 0) {
    d /= 2ll;
    r += 1ll;
  }

  // Wikipedia says these are sufficient for my input size.
  if (!do_miller_rabin_test(n, d, r, 2))
    return FALSE;
  if (!do_miller_rabin_test(n, d, r, 3))
    return FALSE;
  if (!do_miller_rabin_test(n, d, r, 5))
    return FALSE;
  if (!do_miller_rabin_test(n, d, r, 7))
    return FALSE;
  return TRUE;
}

int main() {
  long final_count = 0L;
  for (long i = 2L; i < LIMIT; i++) {
    if (!is_prime(i)) {
      continue;
    }
    if (i * i >= LIMIT) {
      break;
    }
    for (long j = i; j < LIMIT; j++) {
      if (!is_prime(j)) {
        continue;
      }
      if (i * j >= LIMIT) {
        break;
      }
      final_count++;
    }
  }
  printf("%ld\n", final_count);
}
