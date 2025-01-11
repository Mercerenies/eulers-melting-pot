
// Same as problem198_1.cpp but using a sort of breadth-first search
// approach so we don't unnecessarily iterate way too many times.
//
// Runs in 8 seconds.

#include <stack>
#include <ostream>
#include <iostream>
#include <unordered_set>

// IMPORTANT: Always assumes the fraction is written in reduced form!
struct Fraction {
  long n;
  long d;
};

Fraction reduce(Fraction ab);

template <>
struct std::hash<Fraction> {
  std::size_t operator()(const Fraction& frac) const noexcept {
    auto h1 = std::hash<long>{}(frac.n);
    auto h2 = std::hash<long>{}(frac.d);
    return h1 ^ (h2 << 1);
  }
};

std::ostream& operator<<(std::ostream& out, Fraction ab) {
  return out << ab.n << "/" << ab.d;
}

auto operator<=>(Fraction ab, Fraction cd) {
  auto [a, b] = ab;
  auto [c, d] = cd;
  return a * d <=> b * c;
}

bool operator==(Fraction ab, Fraction cd) {
  return ab <=> cd == 0;
}

long gcd(long a, long b) {
  while (b != 0) {
    long t = a % b;
    a = b;
    b = t;
  }
  return a;
}

Fraction reduce(Fraction ab) {
  long d = gcd(ab.n, ab.d);
  return {.n = ab.n / d, .d = ab.d / d};
}

// Precondition: The denominators are not equal.
Fraction mediant(Fraction ab, Fraction cd) {
  auto [a, b] = ab;
  auto [c, d] = cd;
  // Claim: Assuming b and d are not equal, this is already in reduced
  // form.
  //
  // Proof: Assume WLOG that b < d. Then the sum b + d < 2 * d. Write
  // x / y for the reduced form of the mediant. We know that the
  // *reduced* denominator y of (a + c) / (b + d) is greater than d,
  // since this mediant is the next term in the Farey sequence (and
  // thus did NOT appear in any smaller Farey sequences, in particular
  // F_d). We know there exists some longeger w with w y = b + d, since
  // y is the reduced denominator. But w d < w y = b + d < 2 d, so w <
  // 2. Hence, w = 1 and the fraction is already in reduced form.
  return {.n = a + c, .d = b + d};
}

// TODO Why do we not need to reduce the mean in our case? Why is the
// mean of two adjacent Farey numbers already in reduced form?
Fraction mean(Fraction ab, Fraction cd) {
  auto [a, b] = ab;
  auto [c, d] = cd;
  return {.n = a * d + b * c, .d = 2 * b * d};
}

// An element of our frontier.
struct SearchPoint {
  Fraction lower;
  Fraction upper;
};

int main() {
  constexpr long DENOMINATOR_BOUND = 100000000;
  constexpr Fraction UPPER_BOUND { 1, 100 };
  long ambiguous_numbers = 0L;
  std::stack<SearchPoint> frontier;

  frontier.push({.lower = {0, 1}, .upper = {1, 50}});
  while (!frontier.empty()) {
    auto next = frontier.top();
    frontier.pop();

    if (next.lower >= UPPER_BOUND) {
      // Prune this node from the search tree; it will never bear
      // fruit.
      continue;
    }

    auto avg = mean(next.lower, next.upper);
    // TODO Why can we short-circuit out here? Why do these
    // denominators of means only strictly increase?
    if (avg.d > DENOMINATOR_BOUND) {
      continue;
    }
    if (avg < UPPER_BOUND) {
      ++ambiguous_numbers;
    }
    if (next.lower.d + next.upper.d <= DENOMINATOR_BOUND) {
      auto mid = mediant(next.lower, next.upper);
      frontier.push({next.lower, mid});
      frontier.push({mid, next.upper});
    }
  }

  std::cout << ambiguous_numbers << std::endl;
}
