
// Note: Overflows and produces an incorrect negative answer

#include <stdio.h>

long long gcd(long long a, long long b) {
  while (b != 0) {
    long long t = b;
    b = a % b;
    a = t;
  }
  return a;
}

long long convergent(long long x) {
  if (x == 0)
    return 2;
  else if (x % 3 != 2)
    return 1;
  else
    return ((x - 2) / 3 + 1) * 2;
}

long long main() {
  long long conv = 99;
  long long num = 1;
  long long denom = 0;
  for (long long i = conv; i >= 0; i--) {
    // Reciprocate
    long long tmp = num;
    num = denom;
    denom = tmp;
    // Add value
    num += denom * convergent(i);
    // Simplify fraction
    num /= gcd(num, denom);
    denom /= gcd(num, denom);
    printf("%lld / %lld\n", num, denom);
  }

  long long result = 0;
  while (num != 0) {
    result += num % 10;
    num /= 10;
  }

  printf("%lld\n", result);
}
