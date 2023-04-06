
// THIS ANALYSIS IS WRONG IGNORE THIS WHOLE FILE!!!!!

// Pascal's triangle is given by the 'choose' function C(n, r)
//
// Notation convenience: Write s(x) for the number of 7's in the prime
// factorization of (x!).
//
// For n < 49, just brute force it. No fancy tricks, just do it.
//
// Once n >= 49, we have C(n, r) = n! / (r! * (n-r)!). The only time
// we DO NOT have divisibility by seven is if s(n) = s(r) + s(n - r).
// That is, if every factor of seven in the numerator is cancelled off
// by one in the denominator.
//
// Looking only at the left half of Pascal's triangle (so r <= n / 2),
// this clearly happens at C(n, n), since C(n, 0) = 1 is not divisible
// by 7. As we slowly increase our r value, s(r) will increase by one
// every seven steps, at least until it hits 49. On the other hand,
// s(n - r) is guaranteed to drop by at least 2 (since 49 has two
// factors of 7) at the position ((n % 7) + 1). Until we hit the
// center of Pascal's triangle, s(r) increases more slowly than s(n -
// r) decreases, so once we've dropped by more than one, we never get
// back up to s(n) again as long as r <= n / 2.
//
// Pascal's triangle is symmetrical, so in total for row n, there are
// 2 * ((n % 7) + 1) values not divisible by seven.

#include <stdio.h>

long ncr(long n, long r) {
  if (2 * r > n) {
    // Do the smaller side of Pascal's triangle, easier to compute.
    r = n - r;
  }
  long numerator = 1L;
  long denominator = 1L;
  for (long i = n - r + 1; i <= n; i++) {
    numerator *= i;
  }
  for (long i = 1; i <= r; i++) {
    denominator *= i;
  }
  return numerator / denominator;
}

long count_for_row(long n) {
  if (1 || (n < 49)) {
    // Brute force
    long total = 0;
    for (long r = 0L; r <= n; r++) {
      printf("%ld ", ncr(n, r));
      if (ncr(n, r) % 7 != 0) {
        total += 1;
      }
    }
    printf("\n");
    return total;
  } else {
    // Clever math ^.^
    return 2 * ((n % 7) + 1);
  }
}

int main() {
  printf("%ld\n", count_for_row(7));
  long count = 0;
  for (long i = 49; i < 100; i++) {
    printf("%ld %ld\n", i, count_for_row(i));
    count += count_for_row(i);
  }
  printf("%ld\n", count);
}
