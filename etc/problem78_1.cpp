
#include <iostream>
#include <vector>

std::vector<int> p_values { 1 };

int pentagonal(int k) {
  return (k * (3 * k - 1)) / 2;
}

int p_value_cache(int n) {
  if (n < 0)
    return 0;
  else if (n == 0)
    return 1;
  else
    return p_values[n];
}

// m = Pinkie Pie
// total = Twilight
// k = Luna
// pentagonal(k) = Angel
// pentagonal(-k) = Rainbow Crash
// sgn = Rarity
// TEMPORARY = Applejack
// TEMPORARY = Rainbow Dash
int gen_next() {
  int m = p_values.size();
  // Compute p(m)
  int total = 0;
  int k = 1;
  while (pentagonal(k) <= m) {
    int sgn = (k % 2 == 0) ? -1 : 1;
    total += sgn * (p_value_cache(m - pentagonal(k)) + p_value_cache(m - pentagonal(-k)));
    total = (total % 1000000 + 1000000) % 1000000;
    k++;
  }
  p_values.push_back(total);
  return p_values[m];
}

int main() {
  while (gen_next() % 1000000 != 0);
  std::cout << p_values.size() - 1 << std::endl;
}
