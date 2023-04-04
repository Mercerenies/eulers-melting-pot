
// :( overflows long long. Trying it in Julia now.

// Miller-Rabin primality for the checks.
//
// According to Wikipedia, Miller-Rabin for the first nine primes is
// good up to well above (150 million)^2.

#include <stdio.h>

typedef int _bool;
#define FALSE 0
#define TRUE 1

#define A_VALUES_COUNT 9
long a_values[A_VALUES_COUNT] = {2, 3, 5, 7, 11, 13, 17, 19, 23};

// Integer powers by repeated squaring. (https://stackoverflow.com/a/101613/2288659)
//
// Modified to be relative to some modulo value.
long ipow(long a0, long b, long k) {
  long long a = a0;
  long long result = 1L;
  while (1) {
    if (b % 2 == 1) {
      result = (result * a) % k;
    }
    b >>= 1;
    if (!b) {
      break;
    }
    a = (a * a) % k;
  }
  return result;
}

_bool miller_rabin_test(long n) {
  // Factor out powers of 2.
  long d = n - 1;
  long s = 0;
  while (d % 2 == 0) {
    d /= 2;
    s += 1;
  }
  printf("%ld %ld %ld", n, d, s);
  // Now do Miler-Rabin for each value of 'a'.
  for (int i = 0; i < A_VALUES_COUNT; i++) {
    long a = a_values[i];
    long x = ipow(a, d, n);
    long y = 1L;
    for (long j = 0; j < s; j++) {
      y = (x * x) % n;
      if ((y == 1) && (x != 1) && (x != n - 1)) {
        return FALSE;
      }
      printf("%ld %ld\n", x, y);
      x = y;
    }
    if (y != 1) {
      printf("Oof %ld %ld\n", y, a);
      return FALSE;
    }
  }
  return TRUE;
}

_bool is_prime(long n) {
  // Check small primes by hand.
  for (long i = 0; i < A_VALUES_COUNT; i++) {
    long p = a_values[i];
    if (n == p) {
      return TRUE;
    }
    if (n % p == 0) {
      return FALSE;
    }
  }
  return miller_rabin_test(n);
}

_bool is_valid(long n) {
  long n2 = n * n;
  if (!is_prime(n2 + 1)) {
    return FALSE;
  }
  if (is_prime(n2 + 2)) {
    return FALSE;
  }
  if (!is_prime(n2 + 3)) {
    return FALSE;
  }
  if (is_prime(n2 + 4)) {
    return FALSE;
  }
  if (is_prime(n2 + 5)) {
    return FALSE;
  }
  if (is_prime(n2 + 6)) {
    return FALSE;
  }
  if (!is_prime(n2 + 7)) {
    return FALSE;
  }
  if (is_prime(n2 + 8)) {
    return FALSE;
  }
  if (!is_prime(n2 + 9)) {
    return FALSE;
  }
  if (is_prime(n2 + 10)) {
    return FALSE;
  }
  if (is_prime(n2 + 11)) {
    return FALSE;
  }
  if (is_prime(n2 + 12)) {
    return FALSE;
  }
  if (!is_prime(n2 + 13)) {
    return FALSE;
  }
  if (is_prime(n2 + 14)) {
    return FALSE;
  }
  if (is_prime(n2 + 15)) {
    return FALSE;
  }
  if (is_prime(n2 + 16)) {
    return FALSE;
  }
  if (is_prime(n2 + 17)) {
    return FALSE;
  }
  if (is_prime(n2 + 18)) {
    return FALSE;
  }
  if (is_prime(n2 + 19)) {
    return FALSE;
  }
  if (is_prime(n2 + 20)) {
    return FALSE;
  }
  if (is_prime(n2 + 21)) {
    return FALSE;
  }
  if (is_prime(n2 + 22)) {
    return FALSE;
  }
  if (is_prime(n2 + 23)) {
    return FALSE;
  }
  if (is_prime(n2 + 24)) {
    return FALSE;
  }
  if (is_prime(n2 + 25)) {
    return FALSE;
  }
  if (is_prime(n2 + 26)) {
    return FALSE;
  }
  if (!is_prime(n2 + 27)) {
    return FALSE;
  }
  return TRUE;
}

int main() {
  // Hard-code 10 as the first solution, because we know by direct
  // computation there are no smaller ones.
  long sum = 0;
  for (long n = 10; n < 1000000; n++) {
    if (is_valid(n)) {
      sum += n;
    }
  }
  printf("%ld\n", sum);
}
