
#include <stdio.h>

int cache[101 * 101] = {0};

int recurse(int n, int biggest) {
  if (cache[n * 101 + biggest] == 0) {
    if (n == 0)
      return 1;
    int total = 0;
    for (int i = (n < biggest) ? n : biggest; i > 0; i--) {
      total += recurse(n - i, i);
    }
    cache[n * 101 + biggest] = total;
  }
  return cache[n * 101 + biggest];
}

int count_all(int n) {
  return recurse(n, n);
}

int main() {
  printf("%d\n", count_all(100) - 1); // Minus one because we exclude the 100 = 100 trivial case.
  return 0;
}
