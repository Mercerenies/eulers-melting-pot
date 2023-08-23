
class Triangle {
  sideLength: i32;
  maxCoord: i32;

  constructor(sideLength: i32) {
    this.sideLength = sideLength;
    this.maxCoord = 4 * this.sideLength;
  }

}

class Point {
  triangle: Triangle;
  a: i32;
  b: i32;
  c: i32;

  constructor(triangle: Triangle, a: i32, b: i32) {
    this.triangle = triangle;
    this.a = a;
    this.b = b;
    this.c = 6 * triangle.sideLength - a - b;
  }

  rotatedLeft(): Point {
    return new Point(this.triangle, this.c, this.a);
  }

  rotatedRight(): Point {
    return new Point(this.triangle, this.b, this.c);
  }

}

// We can't directly store points in a set, so we have to get a bit
// creative. The "Line" class stores an *array* of points and supports
// rotation. Then we can convert it into a set of point-like things
// using toPointSet().
class Line {
  points: Array<Point>

  constructor(points: Array<Point>) {
    this.points = points;
  }

  rotatedLeft(): Line {
    return new Line(this.points.map<Point>((point) => point.rotatedLeft()));
  }

  rotatedRight(): Line {
    return new Line(this.points.map<Point>((point) => point.rotatedRight()));
  }

  // Points can be characterized (for the purposes of equality) as a
  // pair (a, b). a and b range from 0 up to 144 (= 4*36) inclusive,
  // so we store a point in the Set as an integer a * 150 + b.
  toPointSet(): Set<i32> {
    const pointSet = new Set<i32>();
    for (let i = 0; i < this.points.length; i++) {
      const point = this.points[i];
      pointSet.add(point.a * 150 + point.b);
    }
    return pointSet;
  }

}

function intersect<T>(x: Set<T>, y: Set<T>): Set<T> {
  const result = new Set<T>();
  const yElements = y.values();
  for (let i = 0; i < yElements.length; i++) {
    if (x.has(yElements[i])) {
      result.add(yElements[i]);
    }
  }
  return result;
}

abstract class LineType {
  name: string;

  constructor(name: string) {
    this.name = name;
  }

  abstract linesFor(triangle: Triangle): Array<Line>;

}

class A extends LineType {

  constructor() {
    super("A");
  }

  linesFor(triangle: Triangle): Array<Line> {
    const lineCount = triangle.sideLength;
    const result: Array<Line> = [];
    for (let index = 0; index < lineCount; index++) {
      const length = 1 + 2 * (lineCount - index);
      const startA = 2 * index;
      const startB = (triangle.maxCoord / 2) + 2 * index;
      const points: Array<Point> = [];
      for (let j = 0; j < length; j++) {
        points.push(new Point(triangle, startA + 2 * j, startB - j));
      }
      result.push(new Line(points));
    }
    return result;
  }

}

class B extends LineType {

  constructor() {
    super("B");
  }

  linesFor(triangle: Triangle): Array<Line> {
    return new A().linesFor(triangle).map<Line>((line) => line.rotatedRight());
  }

}

class C extends LineType {

  constructor() {
    super("C");
  }

  linesFor(triangle: Triangle): Array<Line> {
    return new A().linesFor(triangle).map<Line>((line) => line.rotatedLeft());
  }

}

class D extends LineType {

  constructor() {
    super("D");
  }

  linesFor(triangle: Triangle): Array<Line> {
    const lineCount = 2 * triangle.sideLength - 1;
    const result: Array<Line> = [];
    for (let index = 0; index < lineCount; index++) {
      const length = Math.min(4 + 3 * index, 4 + 3 * (lineCount - index - 1));
      const a = 2 * (index + 1);
      const startB = (triangle.maxCoord / 2 - 1) - index;
      const points: Array<Point> = [];
      for (let j = 0; j < length; j++) {
        points.push(new Point(triangle, a, startB + j));
      }
      result.push(new Line(points));
    }
    return result;
  }

}

class E extends LineType {

  constructor() {
    super("E");
  }

  linesFor(triangle: Triangle): Array<Line> {
    return new D().linesFor(triangle).map<Line>((line) => line.rotatedRight());
  }

}

class F extends LineType {

  constructor() {
    super("F");
  }

  linesFor(triangle: Triangle): Array<Line> {
    return new D().linesFor(triangle).map<Line>((line) => line.rotatedLeft());
  }

}

function allLineTypes(): Array<LineType> {
  return [
    new A(),
    new B(),
    new C(),
    new D(),
    new E(),
    new F(),
  ];
}

class TriangleClass {
  lineType1: LineType;
  lineType2: LineType;
  lineType3: LineType;
  multiplier: i32;

  constructor(lineType1: LineType, lineType2: LineType, lineType3: LineType, multiplier: i32) {
    this.lineType1 = lineType1;
    this.lineType2 = lineType2;
    this.lineType3 = lineType3;
    this.multiplier = multiplier;
  }

  countTriangles(allLines: Map<string, Array<Set<i32>>>): i32 {
    let sum: i32 = 0;
    const lineClass1 = allLines.get(this.lineType1.name);
    const lineClass2 = allLines.get(this.lineType2.name);
    const lineClass3 = allLines.get(this.lineType3.name);
    for (let i = 0; i < lineClass1.length; i++) {
      const line1 = lineClass1[i];
      for (let j = 0; j < lineClass2.length; j++) {
        const line2 = lineClass2[j];
        if (intersect(line1, line2).size == 0) {
          continue;
        }
        for (let k = 0; k < lineClass3.length; k++) {
          const line3 = lineClass3[k];
          if (intersect(line1, line3).size == 0) {
            continue;
          }
          if (intersect(line2, line3).size == 0) {
            continue;
          }
          if (intersect(line1, intersect(line2, line3)).size > 0) {
            continue;
          }
          sum += 1;
        }
      }
    }
    return sum * this.multiplier;
  }

}

function allTriangleClasses(): Array<TriangleClass> {
  return [
    new TriangleClass(new A(), new B(), new C(), 1),
    new TriangleClass(new D(), new E(), new F(), 1),
    new TriangleClass(new A(), new B(), new F(), 3),
    new TriangleClass(new A(), new E(), new F(), 3),
    new TriangleClass(new A(), new B(), new D(), 6),
    new TriangleClass(new A(), new D(), new E(), 6),
  ]
}

function linesByClass(triangle: Triangle): Map<string, Array<Set<i32>>> {
  const map = new Map<string, Array<Set<i32>>>();
  const lineTypes = allLineTypes();
  for (let i = 0; i < lineTypes.length; i++) {
    const lines = lineTypes[i].linesFor(triangle);
    map.set(lineTypes[i].name, lines.map<Set<i32>>((line) => line.toPointSet()));
  }
  return map;
}

function countTriangles(outerTriangle: Triangle): i32 {
  let sum: i32 = 0;
  const allLines = linesByClass(outerTriangle);
  const triangleClasses = allTriangleClasses();
  for (let i = 0; i < triangleClasses.length; i++) {
    sum += triangleClasses[i].countTriangles(allLines);
  }
  return sum;
}

export function main(): i32 {
  const triangle = new Triangle(36);
  return countTriangles(triangle);
}
