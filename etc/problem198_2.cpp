
// Same as problem198_1.cpp but using a sort of depth-first search
// approach so we don't unnecessarily iterate way too many times.
//
// Runs in 2 seconds.

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

// Claim: As long as we're taking means of adjacent numbers in some
// Farey sequence, then we don't need to reduce the fraction. Put
// another way, the fraction will always have (reduced) denominator
// 2bd.
//
// Lemma: Let a/b and c/d be adjacent numbers in some Farey sequence.
// Then ad-bc = 1.
//
// Proof (Lemma): Theorem 1 in
// https://www.whitman.edu/Documents/Academics/Mathematics/2016/Zukin.pdf
//
// Proof (Claim): The mean of a/b and c/d is written as (ad+bc)/2bd.
// Suppose, for the sake of contradiction, that this fraction is
// reducible. Then there is a prime q which divides both the numerator
// and the denominator. Rewrite the fraction: (ad-bc+2bc)/2bd. By the
// lemma, this is (1+2bc)/2bd.
//
// From this form, we clearly see that the prime q cannot be 2, since
// the numerator is odd. The prime q must divide either b or d then.
// It cannot divide b, for the numerator is 1 plus a multiple of b
// (hence, in that case, 1 plus a multiple of q). So q divides d.
//
// Now consider the original fraction again: (ad+bc)/2bd. q divides
// the numerator and also divides ad, so q divides bc. q cannot divide
// c since c/d is already in reduced terms (hence c and d are
// coprime), so we conclude c divides b. But we already proved that c
// cannot divide b, hence contradiction. Thus, we need not reduce the
// fraction. Q.E.D
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

  // Claim: We will never double-count an ambiguous number with this
  // algorithm (hence, we just need a long, not a hashset, here).
  //
  // Proof: Let (x, y) be the adjacent Farey numbers by which we first
  // encounter an ambiguous number k. Then, per the discussion above
  // the mean() function, if the denominators of x and y are b and d
  // (respectively) then the denominator of k (in reduced form) is
  // 2bd. As we continue to iterate down in the (x, y) range, we will
  // get strictly larger denominators, and thus the denominators of
  // the means will be larger than 2bd. Hence, we cannot generate the
  // number k again. And this is the only branch in the tree capable
  // of producing this number, since this is the only branch which
  // contains numbers in the interval (x, y). Q.E.D
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
    // See the above proof for the claim on mean(). The denominator of
    // avg (in reduced terms) is always 2bd, so as the denominators of
    // our Farey numbers increase, so does the denominator of the
    // mean. Hence, if this denominator is too big, then continuing
    // our search down this branch will only lead to means with larger
    // denominators.
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
