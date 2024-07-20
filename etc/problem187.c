
#include <stdio.h>
#include <stdlib.h>

typedef int _bool;

#define TRUE 1
#define FALSE 0

#define LIMIT 100000000

_bool* sieve_of_eratosthenes(long limit) {
  _bool* result = malloc(limit * sizeof(_bool));
  for (long i = 2; i < limit; i++) {
    result[i] = TRUE;
  }
  for (long i = 2; i < limit; i++) {
    if (result[i]) {
      long j = i + i;
      while (j < limit) {
        result[j] = FALSE;
        j += i;
      }
    }
  }
  return result;
}

long count_bits(_bool* bits, long size) {
  long count = 0L;
  for (long i = 0; i < size; i++) {
    if (bits[i]) {
      count++;
    }
  }
  return count;
}

long* bitmask_to_array(_bool* bits, long size) {
  long count = count_bits(bits, size);
  long* result = malloc(count * sizeof(long));
  long result_index = 0L;
  for (long i = 0; i < size; i++) {
    if (bits[i]) {
      result[result_index] = i;
      result_index++;
    }
  }
  return result;
}

int main() {
  _bool* primes_bitmask = sieve_of_eratosthenes(LIMIT);
  long primes_count = count_bits(primes_bitmask, LIMIT);
  long* primes = bitmask_to_array(primes_bitmask, LIMIT);
  free(primes_bitmask);
  long final_count = 0L;
  for (long i = 0L; i < primes_count; i++) {
    if (primes[i] * primes[i] >= LIMIT) {
      break;
    }
    for (long j = i; j < primes_count; j++) {
      if (primes[i] * primes[j] >= LIMIT) {
        break;
      }
      final_count++;
    }
  }
  printf("%ld\n", final_count);
}
