
#include <stdio.h>

// Straight brute-force. The original equation is
//
// 1/a + 1/b = p/(10^n)
//
// with n fixed. After removing denominators,
//
// 10^n (a + b) = p a b
//
// Solve for p:
//
// p = 10^n (a + b) / (a b)
//
// We want values of a and b which make p a positive integer.
//
// p = 10^n / a + 10^n / b <= 2 (10^n / a) [Recall a <= b]
//
// When a > 2 * 10^n, we have p < 1 by the above, so we can stop.
// That's our bound for a.
//
// Now fix a. We have a <= b by assumption.
//
// p = 10^n / a + 10^n / b
//
// As b increases, the second term decreases. At some point, the
// second term becomes small enough that the right-hand sum will never
// be an integer again. When that happens, stop iterating over b.
//
// Precisely, stop iterating b when there is no integer between (10^n
// / a) and (10^n / a + 10^n / b). The latter is written as
//
// (10^n (a + b)) / (a b)
//
// so stop when (10^n // a) is equal to (10^n (a + b)) // (a b), where
// '//' is integer division.
//
// Claim: If gcd(a, b) = 1 (with 10^n / a + 10^n / b an integer) then a and b divide 10^n
//
// Note 10^n / a + 10^n / b = 10^n (a + b) / (a b), so 10^n (a + b)
// [mod ab] = 0, or 10^n a = 10^n (- b) [mod ab]. But the subgroup
// of [mod ab] generated by a and the one generated by b have
// trivial intersection, so both sides must be zero, hence 10^n a =
// 10^n b = 0 [mod ab]. Equivalently, a and b divide 10^n.
//
// Side note: It feels like I should've been able to prove that
// without bringing in the massive blunt instrument of group theory,
// but meh.
//
// So put another way, all of the "primitive" solutions to this (with
// gcd(a, b) = 1) require that a and b divide 10^n, and every solution
// is a linear multiple of a primitive one.
//
// When looking for primitive solutions, we must have gcd(a, b) = 1.
// Split into two cases. If a = 1 then try all possible b values.
// Otherwise, one of a or b must be a power of 2 and the other must be
// a power of 5.
//
// On top of that, this turns out to be an 'if-and-only-if'. Every
// pair of coprime integers which divides 10^n is a solution. (Proof
// is straightforward; if 10^n / a and 10^n / b are integers then
// their sum clearly is). So just count the factors of each p after
// generating the p values in this way.

long gcd(long a, long b) {
  while (b != 0) {
    long tmp = a;
    a = b;
    b = tmp % b;
  }
  return a;
}

long number_of_factors(long x) {
  long count = 0;
  for (long i = 1; i * i <= x; i++) {
    if (x % i == 0) {
      if (i * i == x) {
        count += 1;
      } else {
        count += 2;
      }
    }
  }
  return count;
}

long count_for(long ten_n, long a, long b) {
  // If p = 10^n * (a + b) / (a * b) is an integer, count all factors
  // of p as solutions. Else, return 0.
  long numer = ten_n * (a + b);
  long denom = a * b;
  // Found a primitive solution. Now every factor of p
  // (including 1 and p) gives a new solution.
  long p = numer / denom;
  return number_of_factors(p);
}

long count_solutions(long ten_n) {
  long count = 0;

  // First, let a = 1 and try all b values. The b values must be a
  // combination of a power of 2 and a power of 5.
  for (long t = 1; ten_n % t == 0; t *= 2) {
    for (long f = 1; ten_n % (t * f) == 0; f *= 5) {
      count += count_for(ten_n, 1, t * f);
    }
  }

  // Now, let a be a nontrivial power of 2 and b a nontrivial power of
  // 5.
  for (long a = 2; ten_n % a == 0; a *= 2) {
    for (long b = 5; ten_n % (a * b) == 0; b *= 5) {
      if (b < a) {
        continue;
      }
      count += count_for(ten_n, a, b);
    }
  }

  // Finally, let b be a nontrivial power of 2 and a a nontrivial power of
  // 5.
  for (long a = 5; ten_n % a == 0; a *= 5) {
    for (long b = 2; ten_n % (a * b) == 0; b *= 2) {
      if (b < a) {
        continue;
      }
      count += count_for(ten_n, a, b);
    }
  }

  return count;
}

int main() {
  long ten_n = 10;
  long total_solutions = 0;
  for (int i = 1; i <= 9; i++) {
    total_solutions += count_solutions(ten_n);
    ten_n *= 10;
  }
  printf("%ld\n", total_solutions);
}
