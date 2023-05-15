
// Direct computation in Scala, with ArrayBuffer rather than HashMap
// (we remove duplicates at the end of each iteration, not during). 30
// seconds, runs correctly.

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer
import scala.math.min

def gcd(_a: Long, _b: Long): Long =
  var a = _a
  var b = _b
  while (b != 0) {
    val tmp = a
    a = b
    b = tmp % a
  }
  a

class Fraction(
  _numerator: Long,
  _denominator: Long,
) {

  val numerator = _numerator / gcd(_numerator, _denominator)
  val denominator = _denominator / gcd(_numerator, _denominator)

  override def equals(that: Any) =
    that match {
      case that: Fraction => {
        (this.numerator, this.denominator) == (that.numerator, that.denominator)
      }
      case _ => false
    }

  override def toString: String =
    s"${numerator}/${denominator}"

  override def hashCode: Int =
    ("Fraction", this.numerator, this.denominator).hashCode

  def *(that: Fraction): Fraction =
    Fraction(this.numerator * that.numerator, this.denominator * that.denominator)

  def recip: Fraction =
    Fraction(this.denominator, this.numerator)

  def /(that: Fraction): Fraction =
    this * that.recip

  def +(that: Fraction): Fraction =
    Fraction(this.numerator * that.denominator + that.numerator * this.denominator, this.denominator * that.denominator)

  def negate: Fraction =
    Fraction(- this.numerator, this.denominator)

  def -(that: Fraction): Fraction =
    this + that.negate

  def |+|(that: Fraction): Fraction =
    (this.recip + that.recip).recip

}

object Fraction {
  given LongToFraction: Conversion[Long, Fraction] with
    def apply(x: Long) = Fraction(x, 1)
  given IntToFraction: Conversion[Int, Fraction] with
    def apply(x: Int) = Fraction(x, 1)
}

def upTo(n: Int): Set[Fraction] = {
  val distinct = ArrayBuffer(Set(Fraction(1, 1)))
  2 to n foreach { x =>
    val newDistinct: ArrayBuffer[Fraction] = ArrayBuffer()
    1 to (x / 2) foreach { n1 =>
      distinct(n1-1).foreach { c1 =>
        val n2 = x - n1
        distinct(n2-1).foreach { c2 =>
          newDistinct ++= List(c1 + c2, c1 |+| c2)
        }
      }
    }
    distinct += newDistinct.toSet
  }
  distinct.flatten.toSet
}

@main def main() = {
  println(upTo(18).size)
}
