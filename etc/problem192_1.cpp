
// Same naive function as problem192.cpp, just without the faulty
// optimization.

#include <cmath>
#include <iostream>

struct Approximation {
  long r;
  long s;
};

long gcd(long a, long b) {
  while (b > 0) {
    long t = b;
    b = a % b;
    a = t;
  }
  return a;
}

Approximation best_approximation(long n, long d) {
  double sqrt_n = std::sqrt(n);
  Approximation best {.r = 0, .s = 1};
  double best_distance = n;
  for (long s = 1; s <= d; s++) {
    long r = 0;
    while ((double)(r - 1) / s < sqrt_n) {
      const double distance = std::abs(sqrt_n - (double)r / s);
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
  const long n = 13;
  for (long d = 1; d < 50; d++) {
    auto best = best_approximation(n, d);
    std::cout << "n = " << n << ", d = " << d << "; then r = " << best.r << ", s = " << best.s << std::endl;
  }
  //auto best = best_approximation(n, 1'000'000'000'000L);
  //std::cout << "n = " << n << ", d = " << 1'000'000'000'000L << "; then r = " << best.r << ", s = " << best.s << std::endl;
  return 0;
}
