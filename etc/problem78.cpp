
#include <iostream>
#include <vector>

std::vector<int> p_values { 1 };

int pentagonal(int k) {
  return (k * (3 * k - 1)) / 2;
}

int p_value(int n) {
  if (n < 0)
    return 0;
  else if (n == 0)
    return 1;
  for (int m = p_values.size(); m <= n; m++) {
    // Compute p(m)
    int total = 0;
    int k = 1;
    while (pentagonal(k) <= m) {
      int sgn = (k % 2 == 0) ? -1 : 1;
      total += sgn * (p_value(m - pentagonal(k)) + p_value(m - pentagonal(-k)));
      total = (total % 1000000 + 1000000) % 1000000;
      k++;
    }
    p_values.push_back(total);
  }
  return p_values[n];
}

int main() {
  int i = 0;
  while (p_value(i) % 1000000 != 0)
    i++;
  std::cout << i << std::endl;
}
