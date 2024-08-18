
// Notes: Given n, our goal is to find the best rational approximation
// r/s, with s <= d. That is, we want to minimize the distance between
// sqrt(n) and r/s. Squaring two quantities is monotonic, so this is
// equivalent to minimizing the distance between and r^2/s^2, or
// minimizing the distance between n s^2 and r^2 (with s and r
// coprime).
//
// In our case, n ranges from 2 up to 100,000, and d is fixed at
// 10^12.
//
// Whoops! My bad! The fact that -^2 is monotone is NOT powerful
// enough to preserve distances in the way I want here.

#include <cmath>
#include <iostream>

struct Approximation {
  int r;
  int s;
};

int gcd(int a, int b) {
  while (b > 0) {
    int t = b;
    b = a % b;
    a = t;
  }
  return a;
}

Approximation best_approximation(int n, int d) {
  Approximation best {.r = 0, .s = 1};
  int best_distance = n;
  for (int s = 1; s <= d; s++) {
    int r = 0;
    while ((r - 1) * (r - 1) < n * s * s) {
      std::cout << "**D " << s << " " << r << std::endl;
      const int distance = std::abs(n * s * s - r * r);
      std::cout << "**B " << best_distance << " " << distance << std::endl;
      if (distance < best_distance) {
        best_distance = distance;
        best = {.r = r, .s = s};
      }
      r += 1;
    }
  }
  return best;
}

int main() {
  const int n = 13;
  for (int d = 1; d < 50; d++) {
    auto best = best_approximation(n, d);
    std::cout << "n = " << n << ", d = " << d << "; then r = " << best.r << ", s = " << best.s << std::endl;
  }
  return 0;
}
