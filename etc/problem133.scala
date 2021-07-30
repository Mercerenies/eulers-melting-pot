
import scala.collection.mutable.HashSet
import scala.language.postfixOps

def isPrime(n: Long): Boolean =
  if n < 2 then
    false
  else
    2L until n forall { n % _ != 0 }

def powermod(a: Long, b: Long, n: Long): Long =
  var p = 2L
  var x = 1L
  var t = b

  while p * 2 <= b do
    p *= 2

  while p > 0 do
    if t >= p then
      x = (x * a) % n
      t -= p
    p /= 2
    if p > 0 then
      x = (x * x) % n
  x
end powermod

def isEventuallyOne(p: Long): Boolean =
  val visited = HashSet[Long]()
  var number = 10L % p
  while !(visited contains number) do
    visited += number
    number = powermod(number, 10, p)
  number == 1
end isEventuallyOne

def willDivideRep(p: Long): Boolean =
  isEventuallyOne(9 * p)

@main
def main() =
  println(2 until 100_000 filter { p => isPrime(p) && !willDivideRep(p) } sum)
end main
