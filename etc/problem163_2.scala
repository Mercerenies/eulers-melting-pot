
// Okay, bear with me. This one's going to get complicated. I define a
// custom coordinate system for referring to the position of a point
// in the triangle or on its boundary.
//
// Assume the side length of our outermost triangle is S. Then we
// define the coordinate (a, b) as a custom distance function along,
// respectively, the bottom and right sides of the equilateral
// triangle. The coordinates a and b range from 0 up to 4*S,
// inclusive.
//
// For S = 2, the diagram looks something like
//         0  /\  8
//        1  /  \  7
//       2  /    \  6
//   c  3  /      \  5   b
//     4  /--------\  4
//    5  / \      / \  3
//   6  /   \    /   \  2
//  7  /     \  /     \  1
// 8  /       \/       \  0
//    ------------------
//   0  1 2 3 4  5 6 7  8
//            a
//
// Note that a is just the X coordinate, scaled a bit funny. To
// determine b from a point, find the perpendicular bisector of that
// point with the right line segment of the triangle and figure out
// which number that bisector intersects.
//
// c is the same with the leftmost line segment. But c can be derived
// from a and b by the formula: 6*S = a + b + c.
//
// Now we have a representation of coordinates in our triangle. The
// advantage to this fairly strange coordinate system is that every
// intersection point we're interested in can be represented as a pair
// of natural numbers. There's no need for floating points or
// radicals.
//
// Now, note that every line in our triangle is of one of six classes.
// We name them here.
//
// * The A lines are parallel to the bottom line segment of our
// triangle.
//
// * The B lines are parallel to the right line segment of our
// triangle.
//
// * The C lines are parallel to the left line segment of our
// triangle.
//
// * The D lines are perpendicular to the A lines.
//
// * The E lines are perpendicular to the B lines.
//
// * The F lines are perpendicular to the C lines.
//
// Now we do a bit of grunt work to figure out how to generate line
// segments of types A and D. This is just drawing out the diagrams
// and figuring out the bounds. You can do it; I believe in you. :)
//
// Once we have A and D, we can easily rotate those to get B, C, E,
// and F. Rotating a point is easy. To rotate left, convert (a, b, c)
// to (c, a, b). To rotate right, convert (a, b, c) to (b, c, a).
// Again, c is a derived coordinate, but it can be used in the
// rotation calculation easily.
//
// Now we have all of the lines, defined as sets of possible
// intersection points.
//
// A triangle in this geometric figure is defined as three lines with
// the following constraints.
//
// * The three lines are not parallel.
//
// * The three lines' intersection points all fall within the outer
// triangle or on its boundary.
//
// * The three lines do not all intersect at the same point.
//
// By our classification above, there are twenty (= ncr(6, 3))
// possible classes of triangles to consider, but we can use symmetry
// to narrow this down to 6. ABC and DEF are their own classes.
//
// ABF = BCD = ACE, the class of two parallels and an opposite
// perpendicular. So we calculate 3*ABF.
//
// AEF = BDF = CDE, the class of two perpendiculars and an opposite
// parallel. So we calculate 3*AEF.
//
// ABD = ABE = ACD = ACF = BCE = BCF, the class of two parallels and
// an included perpendicular. So we calculate 6*ABD.
//
// ADE = ADF = BDE = BDF = CDF = CEF, the class of two perpendiculars
// and an included parallel. So we calculate 6*ADE.
//
// Count triangles for each class by brute force using the algorithm
// above, multiply by appropriate symmetries, sum the result, and
// we're done.
//
// Compiles + runs in 5 seconds. I have not run those two steps
// separately.

object problem163_2 {

  case class Triangle(
    val sideLength: Int,
  ) {

    // a, b, and c range from 0 up to this value inclusive.
    def maxCoord: Int = 4 * sideLength

    case class Point(
      val a: Int,
      val b: Int,
    ) {

      // Derived third coordinate.
      val c: Int = 6 * sideLength - a - b

      def rotatedLeft: Point = Point(c, a)

      def rotatedRight: Point = Point(b, c)

    }

    // A line is defined as the set of points of interest on that
    // line.
    case class Line(
      val points: Set[Point],
    ) {

      infix def intersect(that: Set[Point]): Set[Point] =
        this.points intersect that

      infix def intersect(that: Line): Set[Point] =
        this.points intersect that.points

      def rotatedLeft: Line =
        Line(points map { _.rotatedLeft })

      def rotatedRight: Line =
        Line(points map { _.rotatedRight })

    }

    object Line {

      def apply(points: Iterable[Point]): Line =
        Line(points.toSet)

      extension(self: Set[Point])
        infix def intersect(that: Line): Set[Point] =
          that intersect self

    }

  }

  // A type of line, defined by its angle. We letter these from A to
  // F, as defined at the top of this file.
  sealed trait LineType {
    def linesFor(triangle: Triangle): List[triangle.Line]
  }

  object LineType {

    val all: List[LineType] = List(A, B, C, D, E, F)

    case object A extends LineType {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        (0 until lineCount(triangle) map { lineFor(triangle, _) }).toList

      private def lineCount(triangle: Triangle): Int = triangle.sideLength

      private def lineFor(triangle: Triangle, index: Int): triangle.Line = {
        val length = 1 + 2 * (lineCount(triangle) - index)
        val startA = 2 * index
        val startB = (triangle.maxCoord / 2) + 2 * index
        triangle.Line(0 until length map { j => triangle.Point(startA + 2 * j, startB - j) })
      }

    }

    case object B extends LineType {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        A.linesFor(triangle).map { _.rotatedLeft }

    }

    case object C extends LineType {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        A.linesFor(triangle).map { _.rotatedRight }

    }

    case object D extends LineType {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        (0 until lineCount(triangle) map { lineFor(triangle, _) }).toList

      private def lineCount(triangle: Triangle): Int = 2 * triangle.sideLength - 1

      private def lineFor(triangle: Triangle, index: Int): triangle.Line = {
        val length = math.min(4 + 3 * index, 4 + 3 * (lineCount(triangle) - index - 1))
        val a = 2 * (index + 1)
        val startB = (triangle.maxCoord / 2 - 1) - index
        triangle.Line(0 until length map { j => triangle.Point(a, startB + j) })
      }

    }

    case object E extends LineType {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        D.linesFor(triangle).map { _.rotatedLeft }

    }

    case object F extends LineType {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        D.linesFor(triangle).map { _.rotatedRight }

    }

  }

  // A class of triangle shapes we need to count, together with what
  // we should multiply it by for symmetry purposes.
  case class TriangleClass(
    val lineType1: LineType,
    val lineType2: LineType,
    val lineType3: LineType,
    val multiplier: Int,
  ) {

    def countTriangles(outerTriangle: Triangle, allLines: Map[LineType, List[outerTriangle.Line]]): Int =
      (for {
        line1 <- allLines(lineType1)
        line2 <- allLines(lineType2)
        if !(line1 intersect line2).isEmpty
        line3 <- allLines(lineType3)
        if !(line1 intersect line3).isEmpty
        if !(line2 intersect line3).isEmpty
        if (line1 intersect line2 intersect line3).isEmpty
      } yield {
        1
      }).sum * multiplier

  }

  object TriangleClass {

    val all: List[TriangleClass] = {
      import LineType.*
      List(
        TriangleClass(A, B, C, 1),
        TriangleClass(D, E, F, 1),
        TriangleClass(A, B, F, 3),
        TriangleClass(A, E, F, 3),
        TriangleClass(A, B, D, 6),
        TriangleClass(A, D, E, 6),
      )
    }

  }

  // Pre-compute and store all lines for a given line type, so we
  // don't recompute for each TriangleClass.
  def linesByClass(triangle: Triangle): Map[LineType, List[triangle.Line]] =
    LineType.all.map { lineClass => lineClass -> lineClass.linesFor(triangle) }.toMap

  def countTriangles(outerTriangle: Triangle): Int = {
    val allLines = linesByClass(outerTriangle)
    TriangleClass.all.map { _.countTriangles(outerTriangle, allLines) }.sum
  }

  @main def main() = {
    val triangle = Triangle(36)
    println(countTriangles(triangle))
  }

}
