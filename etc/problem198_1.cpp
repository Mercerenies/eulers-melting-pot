
// https://en.wikipedia.org/wiki/Farey_sequence#Farey_neighbours
//
// Let F_n be a Farey sequence. Then take two adjacent terms x and y
// in the sequence F_n. The arithmetic mean of these two terms is an
// ambiguous number. This is because the Farey sequence consists of
// all rational numbers with denominator at most n, so the two terms x
// and y are equally good approximations of our mean.
//
// The converse is also true. Let w be an ambiguous number which has
// two equally good rational approximations with denominator at most
// n. Then consider the nth Farey sequence F_n. The two rational
// approximations necessarily appear adjacent to each other in F_n,
// and w is their arithmetic mean.
//
// Can't even do up to denominator 20,000. Definitely too slow.

#include <list>
#include <ostream>
#include <iostream>
#include <unordered_set>

template <typename C>
class PrintableList {
private:
  const C& container;
public:
  PrintableList(const C& c) : container(c) {}

  friend std::ostream& operator<<(std::ostream& out, const PrintableList<C>& pl) {
    bool first = true;
    out << "[";
    for (const auto& elem : pl.container) {
      if (!first) {
        out << ", ";
      }
      first = false;
      out << elem;
    }
    out << "]";
    return out;
  }
};

struct Fraction {
  int n;
  int d;
};

Fraction reduce(Fraction ab);

template <>
struct std::hash<Fraction> {
  std::size_t operator()(const Fraction& frac) const noexcept {
    auto frac1 = reduce(frac);
    auto h1 = std::hash<int>{}(frac1.n);
    auto h2 = std::hash<int>{}(frac1.d);
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

int gcd(int a, int b) {
  while (b != 0) {
    int t = a % b;
    a = b;
    b = t;
  }
  return a;
}

Fraction reduce(Fraction ab) {
  int d = gcd(ab.n, ab.d);
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
  // F_d). We know there exists some integer w with w y = b + d, since
  // y is the reduced denominator. But w d < w y = b + d < 2 d, so w <
  // 2. Hence, w = 1 and the fraction is already in reduced form.
  return {.n = a + c, .d = b + d};
}

Fraction mean(Fraction ab, Fraction cd) {
  auto [a, b] = ab;
  auto [c, d] = cd;
  return reduce({.n = a * d + b * c, .d = 2 * b * d});
}

// upper_limit is exclusive.
void generate_next_row(std::list<Fraction>& row, int max_denom) {
  // Generate the new terms.
  auto iter = std::begin(row);
  auto end = std::end(row);
  while (true) {
    auto iter_a = iter;
    auto iter_b = ++iter;
    if (iter_b == end) {
      break;
    }
    auto mid = mediant(*iter_a, *iter_b);
    if (mid.d <= max_denom) {
      row.insert(iter_b, mid);
    }
  }
}

void find_means(std::unordered_set<Fraction>& ambiguous_numbers,
                const std::list<Fraction>& row,
                Fraction upper_limit) {
  auto iter = std::begin(row);
  auto end = std::end(row);
  while (iter != end) {
    auto iter_a = iter;
    auto iter_b = ++iter;
    auto mid = mean(*iter_a, *iter_b);
    if (mid < upper_limit) {
      ambiguous_numbers.insert(mid);
    }
  }
}

int main() {
  std::unordered_set<Fraction> ambiguous_numbers;
  std::list<Fraction> sequence {
    { 0, 1 },
    { 1, 50 }, // Start at 1/50, since the average of this and 0 is
               // our actual upper bound of 1/100.
  };
  int denom = 50;
  while (denom < 20000) {
    if (denom % 100 == 0) {
      std::cout << denom << std::endl;
    }
    ++denom;
    generate_next_row(sequence, denom);
    find_means(ambiguous_numbers, sequence, Fraction{1, 100});
  }
  std::cout << ambiguous_numbers.size() << std::endl;
}
