
object problem163_2 {

  case class Triangle(
    val sideLength: Int,
  ) {

    def maxCoord: Int = 4 * sideLength

    case class Point(
      val a: Int,
      val b: Int,
    ) {
      val c: Int = 6 * sideLength - a - b

      def rotatedLeft: Point = Point(c, a)

      def rotatedRight: Point = Point(b, c)

    }

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
