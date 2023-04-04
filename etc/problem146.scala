
// I have no illusions about being able to brute force the whole
// problem. But I want to see the first few numbers that satisfy this
// pattern, so we can check OEIS.

def divides(a: Long, b: Long): Boolean =
  b % a == 0

def isPrime(n: Long): Boolean =
  !(2L to math.ceil(math.sqrt(n.toDouble)).toLong exists { divides(_, n) })

def isPossible(n: Long): Boolean =
  List(3, 7, 9, 13, 27) forall { !divides(_, n) }

def satisfiesPattern(n: Long): Boolean =
  (List(n * n + 1, n * n + 3, n * n + 7, n * n + 9, n * n + 13, n * n + 27) forall { isPrime(_) }) &&
    (List(n * n + 2, n * n + 4, n * n + 5, n * n + 6, n * n + 8,
      n * n + 10, n * n + 11, n * n + 12, n * n + 14, n * n + 15, n * n + 16,
      n * n + 17, n * n + 18, n * n + 19, n * n + 20, n * n + 21, n * n + 22,
      n * n + 23, n * n + 24, n * n + 25, n * n + 26) forall { !isPrime(_) })

/*
@main def main() = {
  1 until 1000000 foreach { n =>
    if (n % 10000 == 0) { println(n) }
    if (isPossible(n)) {
      if (satisfiesPattern(n)) {
        println(n)
      }
    }
  }
}
*/

val LIMIT = 1000000

@main def main() = {
  var total = 0
  0 until (1 + LIMIT / 210) foreach { k =>
    List(10, 80, 130, 200) foreach { i =>
      val n = 210 * k + i
      if (satisfiesPattern(n)) {
        println(n)
        total += n
      }
    }
  }
  println(total)
}

// Up to 1 million:
// 10
// 315410
// 927070
//
// Progression between the primes: 2, 4, 2, 4, 14
//
// As a prime k-tuple: (0, 2, 6, 8, 12, 26) [https://en.wikipedia.org/wiki/Prime_k-tuple]
