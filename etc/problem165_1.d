
// Working solution in D. Uses rationals, as floats are subject to
// imprecision and get a bad answer. 29 seconds

module problem165_1;

import std.stdio;
import std.math;
import std.algorithm;
import std.array;
import std.typecons;
import std.numeric;

struct Rational {
  const long numerator;
  const long denominator;

  this(long numerator, long denominator) {
    assert(denominator != 0);
    if (denominator < 0) {
      denominator *= -1;
      numerator *= -1;
    }
    long d = gcd(numerator, denominator);
    this.numerator = numerator / d;
    this.denominator = denominator / d;
  }

  this(long longValue) {
    this(longValue, 1L);
  }

  int opCmp(ref const Rational that) const {
    return cast(int) sgn(this.numerator * that.denominator - that.numerator * this.denominator);
  }

  Rational opUnary(string op : "-")() const {
    return Rational(- numerator, denominator);
  }

  Rational opBinary(string op : "+")(Rational that) const {
    return Rational(this.numerator * that.denominator + that.numerator * this.denominator, this.denominator * that.denominator);
  }

  Rational opBinary(string op : "-")(Rational that) const {
    return this + - that;
  }

  Rational opBinary(string op : "*")(Rational that) const {
    return Rational(this.numerator * that.numerator, this.denominator * that.denominator);
  }

  Rational opBinary(string op : "/")(Rational that) const {
    return this * that.recip;
  }

}

Rational recip(Rational rat) {
  return Rational(rat.denominator, rat.numerator);
}

int rsgn(Rational rat) {
  return cast(int) sgn(rat.numerator);
}

struct Point {
  const Rational x;
  const Rational y;

  int opCmp(ref const Point that) {
    if (this.x - that.x != Rational(0, 1)) {
      return rsgn(this.x - that.x);
    } else {
      return rsgn(this.y - that.y);
    }
  }

}

struct LineSegment {
  const Point first;
  const Point second;

  // Normalize so that first <= second.
  this(Point first, Point second) {
    if (first > second) {
      this.second = first;
      this.first = second;
    } else {
      this.first = first;
      this.second = second;
    }
  }

}

bool isVertical(ref const LineSegment segment) {
  return (segment.first.x == segment.second.x);
}

Rational slope(ref const LineSegment segment) {
  assert(!segment.isVertical);
  return (segment.second.y - segment.first.y) / (segment.second.x - segment.first.x);
}

Rational f(ref const LineSegment segment, Rational x) {
  Rational m = segment.slope;
  return m * (x - segment.first.x) + segment.first.y;
}

Nullable!Point intersectionPoint(ref const LineSegment line1, ref const LineSegment line2) {
  // First, deal with the awkwardness of vertical lines (whose slope
  // is undefined).
  if (line1.isVertical && line2.isVertical) {
    // Two vertical lines: Always false
    return Nullable!Point.init;
  }
  if (line2.isVertical) {
    return intersectionPoint(line2, line1);
  }
  if (line1.isVertical) {
    // One vertical line: Special case
    Rational x = line1.first.x;
    Rational y = f(line2, x);
    // Check that it's in bounds for line1
    if ((y <= line1.first.y) || (y >= line1.second.y)) {
      return Nullable!Point.init;
    }
    // Check that it's in bounds for line2
    if ((x <= line2.first.x) || (x >= line2.second.x)) {
      return Nullable!Point.init;
    }
    return Nullable!Point(Point(x, y));
  }

  // General case: Neither line is vertical
  Rational m1 = line1.slope;
  Rational m2 = line2.slope;
  if (m1 == m2) {
    // Parallel or overlapping: Always false
    return Nullable!Point.init;
  }
  Rational x = (line2.first.y - line1.first.y  + m1 * line1.first.x - m2 * line2.first.x) / (m1 - m2);
  Rational y = f(line1, x);
  // Check bounds for line1
  if ((x <= line1.first.x) || (x >= line1.second.x)) {
    return Nullable!Point.init;
  }
  // Check bounds for line2
  if ((x <= line2.first.x) || (x >= line2.second.x)) {
    return Nullable!Point.init;
  }
  return Nullable!Point(Point(x, y));
}

long[] blumBlumShub(int count) {
  long[] numbers = new long[count];
  numbers[0] = 290797L;
  for (int i = 1; i < count; i++) {
    numbers[i] = (numbers[i - 1] * numbers[i - 1]) % 50515093L;
  }
  numbers.each!((ref n) => n %= 500);
  return numbers;
}

LineSegment[] numbersToSegments(long[] numbers) {
  auto segments = appender!(LineSegment[]);
  segments.reserve((numbers.length + 1) / 4);
  for (int i = 1; i < numbers.length; i += 4) {
    Rational x1 = Rational(numbers[i]);
    Rational y1 = Rational(numbers[i + 1]);
    Rational x2 = Rational(numbers[i + 2]);
    Rational y2 = Rational(numbers[i + 3]);
    segments ~= LineSegment(Point(x1, y1), Point(x2, y2));
  }
  return segments[];
}

void runSampleValues() {
  LineSegment line1 = LineSegment(Point(Rational(27), Rational(44)), Point(Rational(12), Rational(32)));
  LineSegment line2 = LineSegment(Point(Rational(46), Rational(53)), Point(Rational(17), Rational(62)));
  LineSegment line3 = LineSegment(Point(Rational(46), Rational(70)), Point(Rational(22), Rational(40)));
  writeln("line1 & line2 == ", intersectionPoint(line1, line2)); // Should be false
  writeln("line1 & line3 == ", intersectionPoint(line1, line3)); // Should be false
  writeln("line2 & line3 == ", intersectionPoint(line2, line3)); // Should be true
}

void main(string[] args) {
  //runSampleValues();
  auto lines = numbersToSegments(blumBlumShub(20001)); // 20,001 = 20,000 + the one starting value of 290797
  int[Point] intersectionPoints; // Used as a set
  long count = 0;
  for (int i = 0; i < lines.length; i++) {
    auto line1 = lines[i];
    for (int j = i + 1; j < lines.length; j++) {
      auto line2 = lines[j];
      auto point = intersectionPoint(line1, line2);
      if (!point.isNull) {
        intersectionPoints[point.get] = 1;
      }
    }
  }
  writeln(intersectionPoints.length);
}
