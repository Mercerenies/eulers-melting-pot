
// Seven levels of abstraction later, and we've got an idea. It can be
// proven that the *only* places pd == 3 can ever happen are along the
// "stitch" where new layers start.
//
// First, ignore the first ring, because it's an annoying corner case.
// We both know 1 and 2 are valid and 3 through 7 are not. Precompute
// that and move on.
//
// Now, assuming the ring number is N (where N=2 is the ring starting
// at 8 and going to 19), the "starting" value is 3N^2 - 3N + 2 and
// the "ending" value is 3N^2 + 3N + 1. These starting/ending values
// are the only places pd == 3 can occur. Everywhere else has two
// differences of 1 and the remaining numbers have two odd differences
// and two even ones.
//
// Now, for the starting values, the differences are 6N+1, 6N-1, and
// 12N+5. For the ending values, the differences are 6N-1, 6N+5, and
// 12N-7. Check all of these for primality.

def isPrime(n: Long): Boolean = (n > 1L) && !((2L to math.sqrt(n).toLong) exists (n % _ == 0L))

def startingValue(n: Long) = 3 * n * n - 3 * n + 2

def endingValue(n: Long) = 3 * n * n + 3 * n + 1

def isStartingValueCorrect(n: Long) = List(6 * n + 1, 6 * n - 1, 12 * n + 5) forall { isPrime(_) }

def isEndingValueCorrect(n: Long) = List(6 * n - 1, 6 * n + 5, 12 * n - 7) forall { isPrime(_) }

def doCalculation(index: Long): Long = {
  // Start our calculation at Ring 2, to avoid awkward corner cases. We
  // know that we're skipping over numbers 1 to 7, which includes
  // exactly two solutions: 1 and 2.
  var remaining = index - 2

  var i = 2L
  while (true) {

    // If i is divisible by five then 5 will divide both 12N+5 and
    // 6N+5, so we can't have a solution.
    if (i % 5L != 0) {
      if (isStartingValueCorrect(i)) {
        remaining -= 1
        if (remaining == 0) {
          return startingValue(i)
        }
      }
      if (isEndingValueCorrect(i)) {
        remaining -= 1
        if (remaining == 0) {
          return endingValue(i)
        }
      }
    }

    i += 1L
  }

  throw new Exception("Solution does not fit into Long :(")
}

println(doCalculation(2000L))
