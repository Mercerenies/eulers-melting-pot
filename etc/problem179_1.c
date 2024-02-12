
// Trying to do clever stuff with prime factors. Didn't work. Only
// gets 1/10 of the way in in 4 minutes. Way worse than the naive
// solution.

#include <stdio.h>

#define LIMIT 10000001

int divisor_counts[LIMIT];

int main() {
  divisor_counts[0] = 0;
  divisor_counts[1] = 1;
  divisor_counts[2] = 2;

  for (int i = 3; i < LIMIT; i++) {
    // For each i, find the smallest nontrivial prime factor p, then
    // find its valuation and split i into two parts: the power of the
    // prime p and the rest of the number. These are coprime.
    if (i % 100000 == 0) {
      printf("%d\n", i);
    }
    int p = 2;
    while (p < i) {
      if (i % p == 0) {
        break;
      }
      p++;
    }
    int pow = 1;
    int product = p;
    while (i % (product * p) == 0) {
      product *= p;
      pow++;
    }
    divisor_counts[i] = (pow + 1) * divisor_counts[i / product];
  }

  int total = 0;
  for (int n = 2; n < 10000000; n++) {
    if (divisor_counts[n] == divisor_counts[n + 1]) {
      total++;
    }
  }
  printf("%d\n", total);
  return 0;
}
