
// Some tricks with prime numbers and precomputing factors using a
// sieve technique. Runs in 0.4 seconds.

#include <stdio.h>

#define LIMIT 10000001

int largest_prime[LIMIT];
int divisors[LIMIT];

int main() {
  // For every number i from 1 up to (exclusive) LIMIT, set
  // largest_prime[i] to be the largest prime number that divides into
  // i.
  largest_prime[0] = 0;
  largest_prime[1] = 0;
  for (int i = 2; i < LIMIT; i++) {
    if (largest_prime[i] == 0) {
      // Update all multiples of the prime i.
      for (int j = i; j < LIMIT; j += i) {
        if (largest_prime[j] < i) {
          largest_prime[j] = i;
        }
      }
    }
  }
  // Let tau(n) be the number of divisors of n. tau is a
  // multiplicative function. That is, tau(a b) = tau(a) tau(b) when a
  // and b are coprime. So set divisors[i] to equal tau(i) for all i,
  // using the fact that we can separate each i into a prime power and
  // the rest of the number.
  divisors[0] = 0;
  divisors[1] = 1;
  for (int i = 2; i < LIMIT; i++) {
    int p = largest_prime[i];
    int valuation = 1;
    int prime_power = p;
    while (i % (prime_power * p) == 0) {
      prime_power *= p;
      valuation++;
    }
    divisors[i] = divisors[i / prime_power] * (valuation + 1);
  }
  // Now iterate and solve the problem directly.
  int total = 0;
  for (int n = 1; n < LIMIT - 1; n++) {
    if (divisors[n] == divisors[n + 1]) {
      total++;
    }
  }
  printf("%d\n", total);
  return 0;
}
