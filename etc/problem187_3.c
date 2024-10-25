
// problem187.c but with stupider multiplication, so we can test the
// approach we'll want to use in Minus using a more reasonable
// environment.
//
// Also converting all longs to ints, since that's what we have in
// Minus.

#include <stdio.h>
#include <stdlib.h>

typedef int _bool;

#define TRUE 1
#define FALSE 0

#define LIMIT 100000000

// Multiplication using the repeated squaring algorithm that we
// usually use for exponents.
int stupid_multiplication(int a, int b) {
  int result = 0;
  while (b > 1) {
    if (b % 2 == 0) {
      a += a;
      b /= 2;
    } else {
      result += a;
      b -= 1;
    }
  }
  result += a;
  return result;
}

int mod2(int b) {
  while (b > 1024) {
    b -= 1024;
  }
  while (b > 512) {
    b -= 512;
  }
  while (b > 256) {
    b -= 256;
  }
  while (b > 128) {
    b -= 128;
  }
  while (b > 64) {
    b -= 64;
  }
  while (b > 32) {
    b -= 32;
  }
  while (b > 16) {
    b -= 16;
  }
  while (b > 8) {
    b -= 8;
  }
  while (b > 4) {
    b -= 4;
  }
  while (b > 2) {
    b -= 2;
  }
  return b;
}

int div2(int b) {
  int x = 0;
  while (b > 1024) {
    b -= 1024;
    x += 512;
  }
  while (b > 512) {
    b -= 512;
    x += 256;
  }
  while (b > 256) {
    b -= 256;
    x += 128;
  }
  while (b > 128) {
    b -= 128;
    x += 64;
  }
  while (b > 64) {
    b -= 64;
    x += 32;
  }
  while (b > 32) {
    b -= 32;
    x += 16;
  }
  while (b > 16) {
    b -= 16;
    x += 8;
  }
  while (b > 8) {
    b -= 8;
    x += 4;
  }
  while (b > 4) {
    b -= 4;
    x += 2;
  }
  while (b > 2) {
    b -= 2;
    x += 1;
  }
  return x;
}

// Multiplication using the repeated squaring algorithm that we
// usually use for exponents, with a dumb division algorithm
int even_stupider_multiplication(int a, int b) {
  int result = 0;
  while (b > 0) {
    if (mod2(b) == 0) {
      a += a;
      b = div2(b);
    } else {
      result += a;
      b -= 1;
    }
  }
  return result;
}

_bool* sieve_of_eratosthenes(int limit) {
  _bool* result = malloc(limit * sizeof(_bool));
  for (int i = 2; i < limit; i++) {
    result[i] = TRUE;
  }
  for (int i = 2; i < limit; i++) {
    if (result[i]) {
      int j = i + i;
      while (j < limit) {
        result[j] = FALSE;
        j += i;
      }
    }
  }
  return result;
}

int count_bits(_bool* bits, int size) {
  int count = 0L;
  for (int i = 0; i < size; i++) {
    if (bits[i]) {
      count++;
    }
  }
  return count;
}

int* bitmask_to_array(_bool* bits, int size) {
  int count = count_bits(bits, size);
  int* result = malloc(count * sizeof(int));
  int result_index = 0L;
  for (int i = 0; i < size; i++) {
    if (bits[i]) {
      result[result_index] = i;
      result_index++;
    }
  }
  return result;
}

int main() {
  printf("%d\n", even_stupider_multiplication(78, 122));
  // Note: We only have to generate primes up to LIMIT / 2, since the
  // "other" prime we're multiplying by is necessarily at least 2.
  _bool* primes_bitmask = sieve_of_eratosthenes(LIMIT / 2);
  int primes_count = count_bits(primes_bitmask, LIMIT / 2);
  int* primes = bitmask_to_array(primes_bitmask, LIMIT / 2);
  free(primes_bitmask);
  int final_count = 0L;
  for (int i = 0L; i < primes_count; i++) {
    printf("%d\n", primes[i]);
    if (even_stupider_multiplication(primes[i], primes[i]) >= LIMIT) {
      break;
    }
    for (int j = i; j < primes_count; j++) {
      if (even_stupider_multiplication(primes[j], primes[i]) >= LIMIT) {
        break;
      }
      final_count++;
    }
  }
  printf("%d\n", final_count);
}
