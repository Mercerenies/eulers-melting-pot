
// Does not appear to be in OEIS.
//
// Runs for 10,000 in under 2 seconds, but we run out of heap memory
// for the real problem.

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import scala.collection.{Seq, Set}

extension (a: Long)
  infix def mod(b: Long) =
    (a % b + b) % b

extension[A] (opt: Option[A])
  def guard(b: Boolean): Option[A] =
    opt.filter { _ => b }

extension[A] (iter: Iterable[A])
  def groupedFrom(groupSizes: Iterable[Int]): Iterable[Iterable[A]] =
    val iterator = iter.iterator
    groupSizes.map { size =>
      iterator.take(size).toList
    }

  def indicesWhere(pred: (A) => Boolean): Iterable[Int] =
    iter.view.zipWithIndex.flatMap { (a, i) => Some(i).guard(pred(a)) }

extension (iter: Iterable[Boolean])
  def indicesTrue: Iterable[Int] =
    iter.indicesWhere { b => b }

def triangularNumber(n: Long): Long =
  n * (n + 1) / 2

case class Pos(y: Int, x: Int) {
  def +(that: Pos) =
    Pos(this.y + that.y, this.x - that.x)

  def -(that: Pos) =
    this + (- that)

  def unary_- =
    Pos(- y, - x)

  def adjacencies: List[Pos] =
    Pos.ChessboardAdjacencies.map(this + _)
}

object Pos {
  val ChessboardAdjacencies = List(
    Pos(-1, -1), Pos(-1,  0), Pos(-1,  1),
    Pos( 0, -1),              Pos( 0,  1),
    Pos( 1, -1), Pos( 1,  0), Pos( 1,  1),
  )
}

trait PrimalityChecker {
  def apply(n: Long): Boolean
}

class PrimesCache extends PrimalityChecker {
  private val cache = HashMap[Long, Boolean]()

  def apply(n: Long): Boolean =
    cache.getOrElseUpdate(n, {
      n >= 2L && (2L until n takeWhile { i => i * i <= n } forall { n % _ != 0L })
    })

  def isPrime(n: Long): Boolean = apply(n)
}

class PrimesSet private (
  private val knownPrimes: Set[Long],
  private val lowerBound: Long,
  private val upperBound: Long,
) extends PrimalityChecker {
  def apply(n: Long): Boolean =
    if (n < 2) {
      false
    } else if (n < lowerBound) {
      throw RuntimeException(f"Out of bounds index ${n} for PrimesSet (n < ${lowerBound})")
    } else if (n >= upperBound) {
      throw RuntimeException(f"Out of bounds index ${n} for PrimesSet (n >= ${upperBound})")
    } else {
      knownPrimes.contains(n)
    }
}

object PrimesSet {
  def applyUnchecked(knownPrimes: Set[Long], lowerBound: Long, upperBound: Long): PrimesSet =
    new PrimesSet(knownPrimes, lowerBound, upperBound)

  def applyUnchecked(knownPrimes: Set[Long], upperBound: Long): PrimesSet =
    applyUnchecked(knownPrimes, 0L, upperBound)

  def fromMaskUnchecked(primesMask: Iterable[Boolean]): PrimesSet =
    applyUnchecked(knownPrimes = primesMask.indicesTrue.map(_.toLong).toSet, upperBound = primesMask.size)
}

object SieveOfEratosthenes {
  def apply(upperBound: Int): PrimalityChecker =
    val primesMask = buildPrimesMask(upperBound)
    PrimesSet.fromMaskUnchecked(primesMask)

  def buildPrimesMask(upperBound: Int): Seq[Boolean] = {
    val primesMask = ArrayBuffer.fill(upperBound)(true)
    primesMask(0) = false
    primesMask(1) = false
    for (i <- 2 to upperBound / 2) {
      if (primesMask(i)) {
        // Anything less than i * i will be marked already by an
        // earlier iteration of this loop.
        var j = i * i
        while (j < upperBound) {
          primesMask(j) = false
          j += i
        }
      }
    }
    primesMask
  }
}

// https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Segmented_sieve
object SegmentedSieveOfEratosthenes {
  def apply(upperBound: Long): PrimalityChecker = {
    val delta = math.sqrt(upperBound.toDouble).toInt
    val knownPrimes = HashSet[Long]()
    val initialPrimesMask = SieveOfEratosthenes.buildPrimesMask(delta)
    addKnownPrimes(knownPrimes, 0L, initialPrimesMask)
    var k = delta
    while (k < upperBound) {
      val primesMask = partialSieve(knownPrimes, k, delta)
      addKnownPrimes(knownPrimes, k, primesMask)
      k += delta
    }
    PrimesSet.applyUnchecked(knownPrimes, upperBound = upperBound)
  }

  private def partialSieve(knownPrimes: HashSet[Long], startingPoint: Long, delta: Int): Seq[Boolean] = {
    val primesMask = ArrayBuffer.fill(delta)(true)
    for (p <- knownPrimes) {
      var j = startingPoint + ((-startingPoint) mod p)
      while (j < startingPoint + delta) {
        primesMask((j - startingPoint).toInt) = false
        j += p
      }
    }
    primesMask
  }

  private def addKnownPrimes(knownPrimes: HashSet[Long], startingPoint: Long, primesMask: Iterable[Boolean]): Unit =
    knownPrimes ++= primesMask.indicesTrue.map { startingPoint + _ }
}

// Modified version of segmented Sieve for when we have a lower bound.
// If we want the primes in a..b, then we don't need to ever look at
// any of the numbers between sqrt(b) and a, since those numbers are
// less than a and their squares are greater than b.
object PartialSieveOfEratosthenes {
  def apply(lowerBound: Long, upperBound: Long): PrimalityChecker = {
    val s = math.sqrt(upperBound.toDouble).toLong + 1L
    val lowPrimes = ArrayBuffer.fill(s.toInt) { true }
    val highPrimes = ArrayBuffer.fill((upperBound - lowerBound).toInt) { true }
    for (i <- 2L until s) {
      if (lowPrimes(i.toInt)) {
        var j = i * i
        while (j < s) {
          lowPrimes(j.toInt) = false
          j += i
        }
        j = lowerBound + ((-lowerBound) mod i)
        while (j < upperBound) {
          highPrimes((j - lowerBound).toInt) = false
          j += i
        }
      }
    }
    val knownPrimesInRange = highPrimes.indicesTrue.map(lowerBound + _).toSet
    PrimesSet.applyUnchecked(knownPrimesInRange, lowerBound = lowerBound, upperBound = upperBound)
  }
}

class Triangle(
  private val rows: Vector[Vector[Long]],
) {
  def apply(y: Int, x: Int): Long =
    rows.lift(y) flatMap { _.lift(x) } getOrElse 0L

  def apply(pos: Pos): Long = apply(pos.y, pos.x)

  def rowCount: Int =
    rows.size

  def rowWidth(row: Int): Int =
    rows.lift(row) map { _.size } getOrElse 0

  def min: Long =
    rows.map(_.min).min

  def max: Long =
    rows.map(_.max).max

  override def toString: String = f"Triangle(${rows})"

  def indices: List[List[Pos]] =
    rows.zipWithIndex.map { (row, y) => (0 until row.size).map { Pos(y, _) }.toList }.toList
}

object Triangle {
  def apply(rows: Iterable[Iterable[Long]]): Triangle =
    new Triangle(rows.map(_.toVector).toVector)

  def consecutive(rowCount: Int): Triangle =
    val entries = 1L to triangularNumber(rowCount)
    Triangle(entries.groupedFrom(1 to rowCount))

  def consecutive(minRow: Int, maxRow: Int): Triangle = {
    val entries = (triangularNumber(minRow) + 1L) to triangularNumber(maxRow)
    Triangle(entries.groupedFrom((minRow + 1) to maxRow))
  }
}

class TriangleWithTriplets private(
  val triangle: Triangle,
  private val tripletsMask: Seq[Seq[Boolean]],
) {
  export triangle.{apply, rowCount, min, max}

  def inPrimeTriplet(pos: Pos): Boolean =
    tripletsMask.lift(pos.y) flatMap { _.lift(pos.x) } getOrElse false

  // Note: This is 1-indexed for consistency with the problem
  // description.
  def s(n: Int): Long =
    val row = n - 1
    if (row >= rowCount - 1) { // Need an extra row since we're checking adjacencies.
      throw IndexOutOfBoundsException(f"${row} > ${rowCount - 1}")
    }
    (0 to triangle.rowWidth(row))
      .filter { col => inPrimeTriplet(Pos(y = row, x = col)) }
      .map { col => this(y = row, x = col) }
      .sum
}

object TriangleWithTriplets {
  def apply(triangle: Triangle): TriangleWithTriplets = {
    val isPrime = PartialSieveOfEratosthenes(triangle.min, triangle.max + 1)
    val indices = triangle.indices
    val mask = indices
      .map(row => ArrayBuffer.fill(row.size) { false })
      .to(ArrayBuffer)
    for (pos <- indices.flatten) {
      if (isCenterOfPrimeTriplet(isPrime, triangle, pos)) {
        mask(pos.y)(pos.x) = true
        for (p <- pos.adjacencies) {
          if (isPrime(triangle(p))) {
            mask(p.y)(p.x) = true
          }
        }
      }
    }
    new TriangleWithTriplets(triangle, mask)
  }

  def consecutive(rowCount: Int): TriangleWithTriplets =
    TriangleWithTriplets(Triangle.consecutive(rowCount))

  def consecutive(minRow: Int, maxRow: Int): TriangleWithTriplets =
    TriangleWithTriplets(Triangle.consecutive(minRow, maxRow))

  private def isCenterOfPrimeTriplet(isPrime: PrimalityChecker, triangle: Triangle, pos: Pos): Boolean =
    isPrime(triangle(pos)) && pos.adjacencies.count { p => isPrime(triangle(p)) } >= 2
}

// Note: Like the above, this is 1-indexed for consistency with the
// problem description.
def s(n: Int): Long = {
  val targetRow = n - 1
  if (targetRow < 2) {
    TriangleWithTriplets.consecutive(targetRow + 3).s(targetRow)
  } else {
    val minRow = targetRow - 2
    val maxRow = targetRow + 3
    TriangleWithTriplets.consecutive(minRow, maxRow).s(3)
  }
}

object problem196 {
  @main def main() = {
    //println(s(5678027) + s(7208785))
    println(s(10000))
    //for (i <- 1 to 10001) {
    //  if (s(i) > 0) {
    //    println(f"s(${i}) = ${s(i)}")
    //  }
    //}
  }
}
