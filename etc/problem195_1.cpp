
// This doesn't check that c is an integer. It definitely doesn't work.

#include <iostream>
#include <cmath>
#include <limits>
#include <functional>

double semiperimeter(double a, double b, double c) {
  return (a + b + c) / 2.0;
}

double inradius_squared(double a, double b, double c) {
  double s = semiperimeter(a, b, c);
  return (s - a) * (s - b) * (s - c) / s;
}

// Uses the assumptions in t() to determine the third side C. Since we
// know our middle angle is 60 degrees, Law of Cosines tells us C^2 =
// A^2 + B^2 - AB.
double inradius_squared(long a, long b) {
  double c = std::sqrt(static_cast<double>(a * a + b * b - a * b));
  return inradius_squared(a, b, c);
}

template <typename T>
T bisection_method(std::function<bool(T)> f, T a, T b) {
  if ((a == b) || (a + 1 == b)) {
    return a;
  }
  auto pivot = (a + b) / 2;
  auto pivot_value = f(pivot);
  if (pivot_value) {
    return bisection_method(f, pivot, b);
  } else {
    return bisection_method(f, a, pivot);
  }
}

template <typename T>
T bisection_method(std::function<bool(T)> f, T a) {
  return bisection_method(f, a, std::numeric_limits<T>::max() / 2);
}

template <typename F, typename T>
T bisection_method(F f, T a) {
  return bisection_method(std::function<bool(T)>(f), a);
}

long t(long n) {
  // Let A, B, C be the side lengths of our triangle, with the only
  // 60-degree angle at the AB vertex. Assume WLOG that A <= B. If A =
  // B then we have an equilateral triangle, which is forbidden by the
  // problem description, so conclude A < B.
  long count = 0L;
  double limit = n * n;
  for (long a = 1L; inradius_squared(a, a + 1) <= limit; a++) {
    long max_b = bisection_method([a, limit](long b) { return inradius_squared(a, b) <= limit; }, a + 1);
    std::cout << a << " " << max_b << std::endl;
    count += max_b - (a + 1) + 1;
  }
  return count;
}

int main() {
  std::cout << t(100) << std::endl;
}
