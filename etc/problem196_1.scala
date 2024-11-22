
// Does not appear to be in OEIS.
//
// 41 seconds to solve the full problem in Scala.

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

  def adjacenciesWithSelf: List[Pos] =
    this :: adjacencies
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

// Singleton object representing the hypothetically-infinite triangle
// of all positive integers.
object Triangle {
  def apply(y: Int, x: Int): Long = {
    if ((y < 0) || (x < 0) || (x >= Triangle.rowWidth(y))) {
      0L
    } else {
      val lastTriangularNumber = triangularNumber(y)
      lastTriangularNumber + x + 1L
    }
  }

  def apply(pos: Pos): Long = apply(pos.y, pos.x)

  def rowWidth(row: Int): Int =
    row + 1
}

class PrimeTripletsChecker(
  val isPrime: PrimalityChecker,
) {
  def isCenterOfPrimeTriplet(pos: Pos): Boolean =
    isPrime(Triangle(pos)) && pos.adjacencies.count { p => isPrime(Triangle(p)) } >= 2

  // Note: This is 0-indexed for consistency with other Scala
  // functions, whereas the problem description is 1-indexed.
  def s(row: Int): Long = {
    val minBoundOnRow = Triangle(row, 0)
    val maxBoundOnRow = Triangle(row, Triangle.rowWidth(row) - 1)
    val primesInAnyTriplet = HashSet[Long]()
    for (y <- (row - 1) to (row + 1)) {
      for (x <- 0 until Triangle.rowWidth(y)) {
        val pos = Pos(y = y, x = x)
        if (isCenterOfPrimeTriplet(pos)) {
          for (p <- pos.adjacenciesWithSelf) {
            val k = Triangle(p)
            if ((k >= minBoundOnRow) && (k <= maxBoundOnRow) && isPrime(k)) {
              primesInAnyTriplet += k
            }
          }
        }
      }
    }
    primesInAnyTriplet.sum
  }
}

object PrimeTripletsChecker {
  def s(row: Int): Long = {
    val minRelevant = Triangle(row - 2, 0)
    val maxRelevant = Triangle(row + 2, Triangle.rowWidth(row + 2) - 1)
    val checker = PrimeTripletsChecker(PartialSieveOfEratosthenes(minRelevant, maxRelevant + 1))
    checker.s(row)
  }
}

object problem196_1 {
  @main def main() = {
    import PrimeTripletsChecker.s
    println(s(5678026) + s(7208784))
  }
}
