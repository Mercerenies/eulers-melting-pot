
// My mistake it's not exponential :(

object problem163_2 {

  case class Triangle(
    val sideLength: Int,
  ) {

    def maxCoord: Int = pow(2, sideLength + 1)

    case class Point(
      val a: Int,
      val b: Int,
    ) {
      val c: Int = 3 * pow(2, sideLength) - a - b

      def rotatedLeft: Point = Point(c, a)

      def rotatedRight: Point = Point(b, c)

    }

    case class Line(
      val points: Set[Point],
    ) {

      def rotatedLeft: Line =
        Line(points map { _.rotatedLeft })

      def rotatedRight: Line =
        Line(points map { _.rotatedRight })

    }

    object Line {

      def apply(points: Iterable[Point]): Line =
        Line(points.toSet)

    }

  }

  sealed trait LineClass {
    def linesFor(triangle: Triangle): List[triangle.Line]
  }

  object LineClass {

    case object A extends LineClass {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        (0 until pow(2, triangle.sideLength) by 2 map { lineFor(triangle, _) }).toList

      private def lineFor(triangle: Triangle, start: Int): triangle.Line =
        triangle.Line(
          start to (triangle.maxCoord - start) by 2 map { a => triangle.Point(a, start + pow(2, triangle.sideLength) - (a - start) / 2) }
        )

    }

    case object B extends LineClass {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        A.linesFor(triangle).map { _.rotatedLeft }

    }

    case object C extends LineClass {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        A.linesFor(triangle).map { _.rotatedRight }

    }

    case object D extends LineClass {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        (2 until triangle.maxCoord by 2 map { lineFor(triangle, _) }).toList

      private def lineFor(triangle: Triangle, a: Int): triangle.Line = {
        val startB = (triangle.maxCoord - a) / 2
        val stopB = math.max(triangle.maxCoord - a, pow(2, triangle.sideLength)) // This is wrong :(
        triangle.Line(startB to stopB map { b => triangle.Point(a, b) })
      }

    }

    case object E extends LineClass {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        E.linesFor(triangle).map { _.rotatedLeft }

    }

    case object F extends LineClass {

      override def linesFor(triangle: Triangle): List[triangle.Line] =
        E.linesFor(triangle).map { _.rotatedRight }

    }

  }

  def pow(a: Int, b: Int): Int = math.pow(a, b).intValue

  @main def main() = {
    val triangle = Triangle(2)
    println(LineClass.A.linesFor(triangle))
    println(LineClass.D.linesFor(triangle))
  }

}
