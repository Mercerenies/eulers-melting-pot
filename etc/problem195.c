
// Too slow to even get through when a = 1.
//
// Also, forgot to check that c is an integer.

#include <stdio.h>
#include <math.h>

double semiperimeter(double a, double b, double c) {
  return (a + b + c) / 2.0;
}

double inradius_squared(double a, double b, double c) {
  double s = semiperimeter(a, b, c);
  return (s - a) * (s - b) * (s - c) / s;
}

// Uses the assumptions in t() to determine the third side C. Since we
// know our middle angle is 60 degrees, Law of Cosines tells us C^2 =
// A^2 + B^2 - AB.
double inradius_squared_ab(long a, long b) {
  double c = sqrt(a * a + b * b - a * b);
  return inradius_squared(a, b, c);
}

long t(long n) {
  // Let A, B, C be the side lengths of our triangle, with the only
  // 60-degree angle at the AB vertex. Assume WLOG that A <= B. If A =
  // B then we have an equilateral triangle, which is forbidden by the
  // problem description, so conclude A < B.
  long count = 0L;
  double limit = n * n;
  for (long a = 1L; inradius_squared_ab(a, a + 1) <= limit; a++) {
    for (long b = a + 1; inradius_squared_ab(a, b) <= limit; b++) {
      printf("%ld %ld %lf\n", a, b, inradius_squared_ab(a, b));
      count++;
    }
  }
  return count;
}

int main() {
  printf("%ld\n", t(100L));
}
