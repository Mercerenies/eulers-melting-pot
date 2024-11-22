
// Too slow, Whiley can't even run 1/100th of the required magnitude.

import std::io
import isqrt from std::math

method modulo(int x, int y) -> int:
    return ((x % y) + y) % y

method triangularNumber(int n) -> int:
    return (n * (n + 1)) / 2

type KnownPrimesSet is {
    bool[] primes,
    int lowerBound
}

method isPrime(KnownPrimesSet primes, int n) -> bool:
    if n < 2:
        return false
    else:
        return primes.primes[n - primes.lowerBound]

method partialSieve(int lowerBound, int upperBound) -> KnownPrimesSet:
    int s = isqrt(upperBound) + 1
    bool[] lowPrimes = [true; s]
    bool[] highPrimes = [true; upperBound - lowerBound]
    int i = 2
    while i < s:
        if lowPrimes[i]:
            int j = i * i
            while j < s:
                lowPrimes[j] = false
                j = j + i
            j = lowerBound + modulo(- lowerBound, i)
            while j < upperBound:
                highPrimes[j - lowerBound] = false
                j = j + i
        i = i + 1
    return { primes: highPrimes, lowerBound: lowerBound }

method rowWidth(int rowNumber) -> int:
    return rowNumber + 1

method triangleValue(int y, int x) -> int:
    if y < 0 || x < 0 || x >= rowWidth(y):
        return 0
    else:
        int lastTriangularNumber = triangularNumber(y)
        return lastTriangularNumber + x + 1

method s(int row) -> int:
    int minRelevant = triangleValue(row - 2, 0)
    int maxRelevant = triangleValue(row + 2, rowWidth(row + 2) - 1)
    KnownPrimesSet primes = partialSieve(minRelevant, maxRelevant)
    io::println(minRelevant)
    io::println(maxRelevant)
    return 0

public export method main():
    //io::println(s(5678026) + s(7208784))
    io::println(s(29999))
