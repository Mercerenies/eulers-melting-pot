
// FAR too inefficient

#include <stdio.h>
#include <stdlib.h>
#include <unordered_map>

#define HITS_SIZE 1500001l

long hits[HITS_SIZE] = {0};
std::unordered_map<long, long> map {};

long l_sqrt(long k2) {
  if (map.find(k2) != map.end())
    return map[k2];
  else
    return 0;
}

int main() {
  for (long i = 0; i < HITS_SIZE; i++) {
    map[i * i] = i;
  }

  for (long i = 1; i < HITS_SIZE; i++) {
    for (long j = i; j < HITS_SIZE; j++) {
      long k2 = i * i + j * j;
      long k = l_sqrt(k2);
      if (k2 >= HITS_SIZE * HITS_SIZE)
        break;
      if ((k > 0) && (i + j + k < HITS_SIZE)) {
        ++hits[ i + j + k ];
      }
    }
    if (i % 10 == 0)
      printf("%ld\n", i);
    if (i * i >= HITS_SIZE * HITS_SIZE)
      break;
  }
  long final_count = 0;
  for (long i = 0; i < HITS_SIZE; i++) {
    if (hits[i] == 1) {
      final_count++;
    }
  }
  printf("%ld\n", final_count);
  return 0;
}
