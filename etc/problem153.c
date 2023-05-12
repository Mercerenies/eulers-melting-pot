
#include <stdio.h>

#define UPPER_LIMIT 100000000

long sums_of_squares[UPPER_LIMIT+1];

long gcd(long a, long b) {
  while (b != 0) {
    long tmp = a;
    a = b;
    b = tmp % b;
  }
  return a;
}

long main() {
  // Init array
  for (long i = 0; i < UPPER_LIMIT+1; i++) {
    // Count each factor as itself (a real number).
    sums_of_squares[i] = i;
  }

  // Calculate all sums of squares. For each complex number (a+bi),
  // this counts it for only the smallest real number that the given
  // complex number divides into.
  for (long a = 0; a * a <= UPPER_LIMIT; a++) {
    for (long b = 1; a * a + b * b <= UPPER_LIMIT; b++) {
      // If gcd(a, b) = 1, then (a+bi)(a-bi) forms a primitive real
      // value. Add the factors (a+bi) and (a-bi) into this primitive
      // factor a^2 + b^2. Then add (2a+2bi) and (2a-2bi) into 2(a^2 +
      // b^2) (which is equal to (2a+2bi)(a-bi) or (a+bi)(2a-2bi)).
      // Repeat this for larger j until primitive*j exceeds our limit.
      if (gcd(a, b) != 1) {
        continue;
      }
      long primitive = a * a + b * b;
      for (long j = 1; j * primitive < UPPER_LIMIT+1; j++) {
        sums_of_squares[j * primitive] += 2 * j * a;
      }
    }
  }

  // Now sum everything, in one big go. We've already figured out the
  // smallest real number each complex divides into, so for each real
  // integer, add it to the sum once for each (real) multiple of that
  // integer that's within our input space.
  long total_sum = 0l;
  for (long i = 1; i < UPPER_LIMIT+1; i++) {
    for (long j = i; j < UPPER_LIMIT+1; j += i) {
      total_sum += sums_of_squares[i];
    }
  }

  printf("%ld\n", total_sum);
  return 0;
}
