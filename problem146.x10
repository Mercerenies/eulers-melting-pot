// -*- Java -*- (close enough)

import java.math.BigInteger;

public class problem146 {

  private static val aValues = [2, 3, 5, 7, 11, 13, 17, 19, 23];
  private static val smallPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43];

  public static def powerMod(var a:BigInteger, var b:BigInteger, k:BigInteger):BigInteger {
    var result:BigInteger = BigInteger.ONE;
    while (true) {
      if (b.mod(BigInteger.valueOf(2)).equals(BigInteger.ONE)) {
        result = result.multiply(a).mod(k);
      }
      b = b.shiftRight(1n);
      if (b.equals(BigInteger.ZERO)) {
        break;
      }
      a = a.multiply(a).mod(k);
    }
    return result;
  }

  public static def millerRabin(n0:long):boolean {
    // Factor out powers of 2.
    val n:BigInteger = BigInteger.valueOf(n0);
    var d:BigInteger = n.subtract(BigInteger.ONE);
    var s:long = 0;
    while (d.mod(BigInteger.valueOf(2)).equals(BigInteger.ZERO)) {
      d = d.divide(BigInteger.valueOf(2));
      s += 1;
    }
    // Now do Miller-Rabin test for each 'a' value.
    for (a in aValues) {
      var x:BigInteger = powerMod(BigInteger.valueOf(a), d, n);
      var y:BigInteger = BigInteger.ONE;
      for (var j:long=0L; j < s; j++) {
        y = x.multiply(x).mod(n);
        if ((y.equals(BigInteger.ONE)) && (!x.equals(BigInteger.ONE)) && (!x.equals(n.subtract(BigInteger.ONE)))) {
          return false;
        }
        x = y;
      }
      if (!y.equals(BigInteger.ONE)) {
        return false;
      }
    }
    return true;
  }

  public static def isPrime(n:long):boolean {
    // Check small primes by hand.
    for (p in smallPrimes) {
      if (n == p) {
        return true;
      }
      if (n % p == 0) {
        return false;
      }
    }
    // Otherwise, do Miller-Rabin.
    return millerRabin(n);
  }
  public static def isValid(n:long):boolean {
    val n2 = n * n;
    if (!isPrime(n2 + 1)) {
      return false;
    }
    if (isPrime(n2 + 2)) {
      return false;
    }
    if (!isPrime(n2 + 3)) {
      return false;
    }
    if (isPrime(n2 + 4)) {
      return false;
    }
    if (isPrime(n2 + 5)) {
      return false;
    }
    if (isPrime(n2 + 6)) {
      return false;
    }
    if (!isPrime(n2 + 7)) {
      return false;
    }
    if (isPrime(n2 + 8)) {
      return false;
    }
    if (!isPrime(n2 + 9)) {
      return false;
    }
    if (isPrime(n2 + 10)) {
      return false;
    }
    if (isPrime(n2 + 11)) {
      return false;
    }
    if (isPrime(n2 + 12)) {
      return false;
    }
    if (!isPrime(n2 + 13)) {
      return false;
    }
    if (isPrime(n2 + 14)) {
      return false;
    }
    if (isPrime(n2 + 15)) {
      return false;
    }
    if (isPrime(n2 + 16)) {
      return false;
    }
    if (isPrime(n2 + 17)) {
      return false;
    }
    if (isPrime(n2 + 18)) {
      return false;
    }
    if (isPrime(n2 + 19)) {
      return false;
    }
    if (isPrime(n2 + 20)) {
      return false;
    }
    if (isPrime(n2 + 21)) {
      return false;
    }
    if (isPrime(n2 + 22)) {
      return false;
    }
    if (isPrime(n2 + 23)) {
      return false;
    }
    if (isPrime(n2 + 24)) {
      return false;
    }
    if (isPrime(n2 + 25)) {
      return false;
    }
    if (isPrime(n2 + 26)) {
      return false;
    }
    if (!isPrime(n2 + 27)) {
      return false;
    }
    return true;
  }


  public static def main(args:Rail[String]):void {
    var sum:long = 0L;
    for (var n:long = 10L; n < 150000000L; n += 10) {
      if (n % 3 == 0) {
        continue;
      }
      if (n % 7 != 3 && n % 7 != 4) {
        continue;
      }
      if (n % 13 != 1 && n % 13 != 3 && n % 13 != 4 && n % 13 != 9 && n % 13 != 10 && n % 13 != 12) {
        continue;
      }
      if (isValid(n)) {
        sum += n;
      }
    }
    Console.OUT.println(sum);
    return;
  }

}
