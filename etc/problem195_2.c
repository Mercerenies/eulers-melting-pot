
// This solution currently guesses at the upper bounds for kappa and
// lambda, and it double counts certain values produced by two
// different (d, kappa, lambda) tuples.

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef int _bool;

typedef struct {
  long a;
  long b;
  long c;
} Triangle;

long gcd(long a, long b) {
  while (b != 0) {
    int tmp = b;
    b = a % b;
    a = tmp;
  }
  return a;
}

double semiperimeter(double a, double b, double c) {
  return (a + b + c) / 2.0;
}

double inradius_squared(double a, double b, double c) {
  double s = semiperimeter(a, b, c);
  return (s - a) * (s - b) * (s - c) / s;
}

Triangle make_triangle(long k, long lambda, long d) {
  Triangle t;
  t.a = d * k * lambda;
  t.b = d * (3 * k * k + lambda * lambda) / 4;
  t.c = d * (2 * k * lambda + labs(3 * k * k - lambda * lambda)) / 4;
  return t;
}

_bool is_equilateral(Triangle t) {
  return (t.a == t.b) && (t.b == t.c);
}

_bool is_actual_triangle(Triangle t) {
  return (t.a + t.b >= t.c);
}

int main() {
  double LIMIT = 100.0;
  double LIMIT_SQUARED = LIMIT * LIMIT;

  long solutions_count = 0L;

  // Iterate over integer-sided triangles with one 60-degree angle
  // using this formula
  // (https://arxiv.org/ftp/arxiv/papers/0803/0803.3778.pdf). Similar
  // to Euclid's formula for generating Pythagorean triples.
  for (long k = 1L;; k++) {
    for (long lambda = 1L;; lambda++) {
      if (gcd(k, lambda) != 1) {
        continue;
      }
      if ((lambda > k) && (lambda < 3 * k)) {
        continue;
      }
      if (is_equilateral(make_triangle(k, lambda, 4))) {
        continue;
      }
      //_bool found_lambda_solutions = 0;
      for (long d = 1L;; d++) {
        if (((k + lambda) % 2 == 1) && (d % 4 != 0)) {
          continue;
        }
        Triangle triangle = make_triangle(k, lambda, d);
        double r_squared = inradius_squared(triangle.a, triangle.b, triangle.c);
        if (r_squared <= LIMIT_SQUARED) {
          printf("k=%ld lam=%ld d=%ld a=%ld b=%ld c=%ld r=%lf\n", k, lambda, d, triangle.a, triangle.b, triangle.c, sqrt(r_squared));
          //found_lambda_solutions = 1;
          solutions_count += 1;
        } else {
          break;
        }
      }
      //if (!found_lambda_solutions) {
      if (lambda > 600) {
        // lambda is too large, break.
        break;
      }
    }
    if (k > 10000) {
      // k too large, we're done.
      break;
    }
  }
  printf("%ld\n", solutions_count);
}
