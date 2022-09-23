
// Just testing, version of problem141_1.raku in C. Use problem141_4.c
// for the real solution :)

#include <math.h>
#include <stdio.h>

int main() {
  long upper_limit = 1000000000000L;
  long total = 0L;
  for (long r = 1L; r + (r + 1L) * (r + 2L) < upper_limit; r++) {
    if (r < 200L || r % 100L == 0L) {
      printf("%ld\n", r);
    }
    for (long d = r + 1L;; d++) {
      long q = (d * d) / r;
      long n = q * d + r;
      if (n > upper_limit) {
        break;
      }
      long nsqrt = sqrt(n);
      if (((d * d) % r == 0) && (nsqrt == floor(nsqrt))) {
        total += n;
      }
    }
  }
  printf("%ld\n", total);
}
