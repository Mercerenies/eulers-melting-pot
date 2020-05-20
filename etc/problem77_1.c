
// The final result is relatively small (as determined by
// problem77.c). Let's see if we can just brute force it in the most
// naive way possible. That would be too convenient.

#include <stdio.h>

int is_prime(int n) {
  if (n < 2)
    return 0;
  for (int i = 2; i < n; i++) {
    if (n % i == 0)
      return 0;
  }
  return 1;
}

int recurse(int n, int biggest) {
  if (n == 0)
    return 1;
  int total = 0;
  for (int i = (n < biggest) ? n : biggest; i > 0; i--) {
    if (is_prime(i))
      total += recurse(n - i, i);
  }
  return total;
}

int count_all(int n) {
  int result = recurse(n, n);
  if (is_prime(n))
    result -= 1;
  return result;
}

int main() {
  for (int i = 0; i < 100; i++) {
    if (count_all(i) > 5000) {
      printf("%d\n", i);
      break;
    }
  }
  return 0;
}
