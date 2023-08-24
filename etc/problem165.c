
// Never mind, I need "distinct" points, and I really, really don't
// want to write a HashSet implementation in C right now.

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define NUMBER_COUNT 20001
#define SEGMENT_COUNT 5000

typedef struct {
  double x;
  double y;
} Point;

typedef struct {
  Point first;
  Point second;
} LineSegment;

Point point(double x, double y) {
  Point point;
  point.x = x;
  point.y = y;
  return point;
}

LineSegment lineSegment(Point first, Point second) {
  // Construct LineSegment such that first <= second (lexicographic
  // ordering). All the functions below implicitly assume that this
  // condition holds.
  if (first.x > second.x) {
    return lineSegment(second, first);
  }
  if ((first.x == second.x) && (first.y > second.y)) {
    return lineSegment(second, first);
  }
  LineSegment segment;
  segment.first = first;
  segment.second = second;
  return segment;
}

bool isVertical(LineSegment* line) {
  return line->first.x == line->second.x;
}

double slope(LineSegment* line) {
  // Precondition: isVertical(line) is false
  return (line->second.y - line->first.y) / (line->second.x - line->first.x);
}

double f(LineSegment* line, double x) {
  // Plug in the input x to the line equation. Precondition:
  // isVertical(line) is false.
  double m = slope(line);
  return m * (x - line->first.x) + line->first.y;
}

bool intersects(LineSegment* line1, LineSegment* line2) {
  // First, deal with the awkwardness with vertical lines (whose slope
  // is undefined).
  if (isVertical(line1) && isVertical(line2)) {
    // Two vertical lines: Always false
    return false;
  }
  if (isVertical(line2)) {
    return intersects(line2, line1);
  }
  if (isVertical(line1)) {
    // One vertical line: Special case
    double x = line1->first.x;
    double y = f(line2, x);
    // Check that it's in bounds for line1
    if ((y <= line1->first.y) || (y >= line1->second.y)) {
      return false;
    }
    // Check that it's in bounds for line2
    if ((x <= line2->first.x) || (x >= line2->second.x)) {
      return false;
    }
    return true;
  }

  // General case: Neither line is vertical
  double m1 = slope(line1);
  double m2 = slope(line2);
  double x = (line2->first.y - line1->first.y  + m1 * line1->first.x - m2 * line2->first.x) / (m1 - m2);
  double y = f(line1, x);
  // Check bounds for line1
  if ((x <= line1->first.x) || (x >= line1->second.x)) {
    return false;
  }
  // Check bounds for line2
  if ((x <= line2->first.x) || (x >= line2->second.x)) {
    return false;
  }
  return true;
}

long* blumBlumShub() {
  long* numbers = malloc(sizeof(long) * NUMBER_COUNT);
  numbers[0] = 290797L;
  for (int i = 1; i < NUMBER_COUNT; i++) {
    numbers[i] = (numbers[i - 1] * numbers[i - 1]) % 50515093L;
  }
  for (int i = 0; i < NUMBER_COUNT; i++) {
    numbers[i] = numbers[i] % 500L;
  }
  return numbers;
}

LineSegment* numbersToSegments(long* numbers) {
  LineSegment* segments = malloc(sizeof(LineSegment) * SEGMENT_COUNT);
  for (int i = 1; i < NUMBER_COUNT; i += 4) {
    double x1 = numbers[i];
    double x2 = numbers[i + 1];
    double y1 = numbers[i + 2];
    double y2 = numbers[i + 3];
    segments[i] = lineSegment(point(x1, y1), point(x2, y2));
  }
  return segments;
}

LineSegment* getSegments() {
  long* numbers = blumBlumShub();
  LineSegment* segments = numbersToSegments(numbers);
  free(numbers);
  return segments;
}

void runSampleValues() {
  LineSegment line1 = lineSegment(point(27, 44), point(12, 32));
  LineSegment line2 = lineSegment(point(46, 53), point(17, 62));
  LineSegment line3 = lineSegment(point(46, 70), point(22, 40));
  printf("line1 & line2 == %d\n", (int)intersects(&line1, &line2)); // Should be false
  printf("line1 & line3 == %d\n", (int)intersects(&line1, &line3)); // Should be false
  printf("line2 & line3 == %d\n", (int)intersects(&line2, &line3)); // Should be true
}

int main() {
  //runSampleValues();
  LineSegment* segments = getSegments();
  
  free(segments);
}
