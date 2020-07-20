
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  // Square root of 5,000,000 is about 7,071, so primes much bigger
  // than that can't possibly help us.
  char composites[7073] = {0};

  composites[0] = 1;
  composites[1] = 1;

  for (int i = 2; i < 7073; i++) {
    if (composites[i] == 0) {
      for (int j = 2 * i; j < 7073; j += i) {
        composites[j] = 1;
      }
    }
  }

  int prime_count = 0;
  for (int i = 2; i < 7073; i++) {
    if (composites[i] == 0)
      prime_count++;
  }
  int* primes = calloc(prime_count, sizeof(int));
  int i = 0;
  for (int j = 2; j < 7073; j++) {
    if (composites[j] == 0)
      primes[i++] = j;
  }

  char* all_values = calloc(50000000, sizeof(char));
  memset(all_values, 0, 50000000);
  for (int ai = 0; ai < prime_count; ai++) {
    int a = primes[ai];
    if (a > 85) // Approximately fourth root of 50,000,000
      break;
    for (int bi = 0; bi < prime_count; bi++) {
      int b = primes[bi];
      if (b > 370) // Approximately cube root of 50,000,000
        break;
      for (int ci = 0; ci < prime_count; ci++) {
        int c = primes[ci];
        // Each summand should be less than 5,000,000, so overflow
        // issues shouldn't be in play.
        int value = a * a * a * a + b * b * b + c * c;
        if (value < 50000000)
          all_values[value] = 1;
      }
    }
  }

  int result = 0;
  for (int i = 0; i < 50000000; i++) {
    if (all_values[i])
      result++;
  }
  printf("%d\n", result);

  free(primes);
  free(all_values);

  return 0;
}
