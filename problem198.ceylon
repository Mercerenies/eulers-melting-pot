
class Fraction(shared Integer numer, shared Integer denom) {}

class SearchPoint(shared Fraction lower, shared Fraction upper) {}

Integer denominatorBound = 100000000;
Integer rangeBound = 100;

shared void run() {
  variable Integer ambiguousNumbers = 0;
  Array<SearchPoint?> frontier = Array<SearchPoint?>.ofSize(20001, null);
  variable Integer frontierIndex = 0;

  frontier[0] = SearchPoint(Fraction(0, 1), Fraction(1, 50));
  while (frontierIndex >= 0) {
    SearchPoint? next = frontier[frontierIndex];
    assert (exists next);
    frontierIndex--;

    Fraction avg = Fraction(
      next.lower.numer * next.upper.denom + next.upper.numer * next.lower.denom,
      next.lower.denom * next.upper.denom * 2
    );
    if (avg.denom > denominatorBound) {
      continue;
    }
    if (rangeBound * avg.numer < avg.denom) {
      ambiguousNumbers++;
    }
    if (next.lower.denom + next.upper.denom <= denominatorBound) {
      Fraction mid = Fraction(next.lower.numer + next.upper.numer, next.lower.denom + next.upper.denom);
      frontier[frontierIndex + 1] = SearchPoint(next.lower, mid);
      frontier[frontierIndex + 2] = SearchPoint(mid, next.upper);
      frontierIndex += 2;
    }
  }

  print(ambiguousNumbers);
}
