
#include <stdio.h>

int perm[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
long long int final = 0;

//   0
//     5  1
//   9   6
// 4  8 7 2
//     3
//

long long int value_of(long long int p, int index) {
  if (perm[index] == 10)
    return p * 100 + 10;
  else
    return p * 10 + perm[index];
}

void validate() {
  if ((perm[0] > perm[1]) || (perm[0] > perm[2]) || (perm[0] > perm[3]) || (perm[0] > perm[4]))
    return;

  int total = perm[0] + perm[5] + perm[6];
  if (total != perm[1] + perm[6] + perm[7])
    return;
  if (total != perm[2] + perm[7] + perm[8])
    return;
  if (total != perm[3] + perm[8] + perm[9])
    return;
  if (total != perm[4] + perm[9] + perm[5])
    return;

  long long int value = 0;
  value = value_of(value, 0);
  value = value_of(value, 5);
  value = value_of(value, 6);
  value = value_of(value, 1);
  value = value_of(value, 6);
  value = value_of(value, 7);
  value = value_of(value, 2);
  value = value_of(value, 7);
  value = value_of(value, 8);
  value = value_of(value, 3);
  value = value_of(value, 8);
  value = value_of(value, 9);
  value = value_of(value, 4);
  value = value_of(value, 9);
  value = value_of(value, 5);
  if ((value > final) && (value < 10000000000000000))
    final = value;

}

void permute(int n) {
  if (n == 1) {
    validate();
    return;
  }

  for (int i = 0; i < n; i++) {
    permute(n - 1);

    if (n % 2 == 1) {
      int tmp = perm[0];
      perm[0] = perm[n - 1];
      perm[n - 1] = tmp;
    } else {
      int tmp = perm[i];
      perm[i] = perm[n - 1];
      perm[n - 1] = tmp;
    }

  }

}

int main() {
  permute(10);
  printf("%lld\n", final);
  return 0;
}
