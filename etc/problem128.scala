
// Wow, it doesn't even get a hundred in, let alone two thousand. I
// expected it to be impossible to brute force, but this is something
// else. But hey, at least we learned that the sequence isn't in OEIS.
//
// First few terms:
// 1, 2, 8, 19, 20, 37, 61, 128, 217, 271, 398, 919

import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.util.control.Breaks._
import scala.language.postfixOps

def isPrime(n: Int): Boolean = (n > 1) && !((2 until n-1) exists (n % _ == 0))

class Cached[A, B](val function: (A) => B) {
  private val cache = HashMap[A, B]()

  def apply(a: A): B = {
    if (!cache.contains(a)) {
      cache(a) = function(a)
    }
    cache(a)
  }

}

val isPrimeCached = Cached(isPrime)

object Cached {
  def apply[A, B](function: (A) => B) = new Cached(function)
}

implicit class ModuloInt(val value: Int) {
  def %%(that: Int) = (value % that + that) % that // "True" modulo
}

trait Dir {
  def move(pos: Pos): Pos
}

object Dir {
  val all = List(North, South, NW, NE, SW, SE)
}

case object North extends Dir {
  override def move(pos: Pos) = Pos(pos.x, pos.y - 1)
}

case object South extends Dir {
  override def move(pos: Pos) = Pos(pos.x, pos.y + 1)
}

case object NW extends Dir {
  override def move(pos: Pos) =
    if (pos.x %% 2 == 0) Pos(pos.x - 1, pos.y - 1) else Pos(pos.x - 1, pos.y)
}

case object NE extends Dir {
  override def move(pos: Pos) =
    if (pos.x %% 2 == 0) Pos(pos.x + 1, pos.y - 1) else Pos(pos.x + 1, pos.y)
}

case object SW extends Dir {
  override def move(pos: Pos) =
    if (pos.x %% 2 == 0) Pos(pos.x - 1, pos.y) else Pos(pos.x - 1, pos.y + 1)
}

case object SE extends Dir {
  override def move(pos: Pos) =
    if (pos.x %% 2 == 0) Pos(pos.x + 1, pos.y) else Pos(pos.x + 1, pos.y + 1)
}

case class Pos(x: Int, y: Int) {
  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)
  def -(that: Pos) = this + - that
  def unary_-() = Pos(- this.x, - this.y)
}

object Pos {
  val zero = Pos(0, 0)
}

class Grid {
  private var buffer = HashMap[Pos, Int]()
  private var reverseLookup = ArrayBuffer[Pos]()
  private var nextRingIndex = 1
  private var nextValue = 2

  buffer(Pos.zero) = 1
  reverseLookup += Pos.zero

  private def addNextRing() = {
    var pos = Pos(0, - nextRingIndex)
    val directions = List(SW, South, SE, NE, North, NW)
    for (d <- directions ; i <- 1 to nextRingIndex) {
      buffer(pos) = nextValue
      reverseLookup += pos
      nextValue += 1
      pos = d.move(pos)
    }
    nextRingIndex += 1
  }

  def apply(pos: Pos) = {
    while (!buffer.contains(pos)) {
      addNextRing()
    }
    buffer(pos)
  }

  def getPosition(index: Int) = {
    while (reverseLookup.length <= index - 1) {
      addNextRing()
    }
    reverseLookup(index - 1)
  }

  def adjacentDifferences(value: Int) = {
    val pos = getPosition(value)
    Dir.all map { d => math.abs(this(d.move(pos)) - value) }
  }

  def pd(value: Int) = adjacentDifferences(value) count { isPrimeCached(_) }


}

/*
val grid = new Grid()
println(1 to 1000 filter { grid.pd(_) == 3 } toList)
*/

val grid = new Grid()
var remainingTiles = 2000
breakable {
  for (i <- 1 to Int.MaxValue) {
    if (grid.pd(i) == 3) {
      remainingTiles -= 1
      println(i, remainingTiles)
    }
    if (remainingTiles == 0) {
      println(i)
      break
    }
  }
}
