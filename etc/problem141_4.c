
// Same as problem141_3.raku, but written in C and using an array
// instead of a hash set. I want to see if we can get it any faster.

#include <math.h>
#include <stdio.h>

#define FOUND_LIST_SIZE 256

typedef int _bool;

_bool is_square(double x) {
  double y = sqrt(x);
  return (y == floor(y));
}

long gcd(long a, long b) {
  while (b != 0) {
    long tmp = a;
    a = b;
    b = tmp % b;
  }
  return a;
}

_bool is_in_list(long needle, long* haystack, int haystack_size) {
  for (int i = 0; i < haystack_size; i++) {
    if (needle == haystack[i]) {
      return 1;
    }
  }
  return 0;
}

int main() {
  long found[FOUND_LIST_SIZE];
  long found_index = 0;

  long limit = 1000000000000L;
  long total = 0L;

  for (long a = 1L; a <= 10000L; a++) {
    if (a % 100L == 0L) {
      printf("%ld\n", a);
    }
    for (long b = 1L; b < a; b++) {
      if (gcd(a, b) > 1) {
        continue;
      }
      if (b * a * a * a + b * b >= limit) {
        break;
      }
      for (long c = 1;; c++) {
        long n = c * c * b * a * a * a + c * b * b;
        if (n >= limit) {
          break;
        }
        if ((is_square(n)) && (!is_in_list(n, found, found_index))) {
          found[found_index++] = n;
          total += n;
        }
      }
    }
  }

  printf("%ld\n", total);

}
