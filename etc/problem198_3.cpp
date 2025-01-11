
// Same as problem198_2.cpp but cutting out a lot of the extra stuff
// to simplify.
//
// Also removed an unnecessary exit condition, which was redundant
// with another that would always trigger first.
//
// This gets us down to under 1 second with -O3 in C++. 23 seconds with -O0

#include <stack>
#include <iostream>

// IMPORTANT: Always assumes the fraction is written in reduced form!
struct Fraction {
  long n;
  long d;
};

// An element of our frontier.
struct SearchPoint {
  Fraction lower;
  Fraction upper;
};

int main() {
  constexpr long DENOMINATOR_BOUND = 100000000;
  constexpr long RANGE_BOUND = 100;
  long ambiguous_numbers = 0L;
  std::stack<SearchPoint> frontier;

  frontier.push({.lower = {0, 1}, .upper = {1, 50}});
  while (!frontier.empty()) {
    auto next = frontier.top();
    frontier.pop();

    Fraction avg {
      .n = next.lower.n * next.upper.d + next.upper.n * next.lower.d,
      .d = next.lower.d * next.upper.d * 2,
    };
    if (avg.d > DENOMINATOR_BOUND) {
      continue;
    }
    if (RANGE_BOUND * avg.n < avg.d) {
      ++ambiguous_numbers;
    }
    if (next.lower.d + next.upper.d <= DENOMINATOR_BOUND) {
      Fraction mid {
        .n = next.lower.n + next.upper.n,
        .d = next.lower.d + next.upper.d,
      };
      frontier.push({next.lower, mid});
      frontier.push({mid, next.upper});
    }
  }

  std::cout << ambiguous_numbers << std::endl;
}
