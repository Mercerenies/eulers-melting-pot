
// Simple brute force, no tricks. 52 seconds in C

#include <stdio.h>

int count_divisors(int value) {
  int count = 0;
  for (int i = 1; i * i <= value; i++) {
    if (value % i == 0) {
      count++;
      if (i * i != value) {
        count++;
      }
    }
  }
  return count;
}

int main() {
  int total = 0;
  int last_count = 2; // The number 2 has two divisors: 1 and itself.
  for (int i = 3; i < 10000001; i++) {
    if (i % 100000 == 0) {
      printf("%d\n", i);
    }
    int current_count = count_divisors(i);
    if (last_count == current_count) {
      total++;
    }
    last_count = current_count;
  }
  printf("%d\n", total);
  return 0;
}
