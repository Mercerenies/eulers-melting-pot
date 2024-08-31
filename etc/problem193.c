
#include <stdlib.h>
#include <stdio.h>

#define BOOL int
#define TRUE 1
#define FALSE 0

// Modified Sieve of Eratosthenes to generate the Mobius function for
// each value from 2 to 2^25.
int* sieve(int limit) {
  int* result = malloc(limit * sizeof(BOOL));
  result[1] = 1;
  for (int i = 2; i < limit; i++) {
    result[i] = -1;
  }
  for (int i = 2; i < limit; i++) {
    if (result[i] == -1) {
      int c = 2;
      for (int j = i + i; j < limit; j += i) {
        if (c % i == 0) {
          result[j] = 0;
        } else {
          result[j] *= -i;
        }
        c++;
      }
    } else if (result[i] < 0) {
      result[i] = 1;
    } else if (result[i] > 0) {
      result[i] = -1;
    }
  }
  return result;
}

int main() {
  const long limit = 1L << 50;
  const int limit_sqrt = 1 << 25;
  int* mobius = sieve(limit_sqrt);

  // Simple summation using Inclusion-Exclusion. Formula comes from
  // here (https://math.stackexchange.com/a/932762/84460), but it's
  // just an efficient way to do Inclusion-Exclusion principle.
  long sum = 0L;
  for (long i = 1; i < limit_sqrt; i++) {
    sum += mobius[i] * (limit / (i * i));
  }
  printf("%ld\n", sum);
}
