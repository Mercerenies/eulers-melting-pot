
// 12ish seconds, success.

using System;
using System.Collections.Generic;

public class problem201_4 {
  private static IDictionary<(int, int, int), bool> cache = new Dictionary<(int, int, int), bool>();
  private static IDictionary<(int, int, int), bool> uniqueCache = new Dictionary<(int, int, int), bool>();

  public static void Main(String[] args) {
    //Console.WriteLine(canUniquelySumTo(5, 2, 2)); // True
    //Console.WriteLine(canUniquelySumTo(5, 2, 3)); // False
    //Console.WriteLine(canUniquelySumTo(89, 8, 3)); // False (non-unique)
    int totalSum = 0;
    // 42,925 = sum of first 50 squares, 295,425 = sum of next 50 squares
    for (int i = 42_925; i <= 295_425; i++) {
      //if (i % 1000 == 0) {
      //  Console.WriteLine(i);
      //}
      if (canUniquelySumTo(i, 100, 50)) {
        //Console.WriteLine(i);
        totalSum += i;
      }
    }
    Console.WriteLine(cache.Count);
    Console.WriteLine(uniqueCache.Count);
    Console.WriteLine(totalSum);
  }

  private static int sumSquares(int n) {
    return n * (n + 1) * (2 * n + 1) / 6;
  }

  private static int sumSquares(int i, int j) {
    return sumSquares(j) - sumSquares(i - 1);
  }

  private static bool canSumTo(int target, int maxTerm, int termCount) {
    if (termCount == 0) {
      return (target == 0);
    } else if (termCount < 0) {
      return false;
    } else if (maxTerm < termCount) {
      return false;
    } else if (target <= 0) {
      return false;
    } else if (target < sumSquares(termCount)) {
      // If the smallest sum we can make is still bigger than the
      // target, then we've overshot.
      return false;
    } else if (target > sumSquares(maxTerm - termCount + 1, maxTerm)) {
      // If the largest sum we can make is still smaller than the
      // target, then we've undershot.
      return false;
    } else {
      if (cache.TryGetValue((target, maxTerm, termCount), out bool result)) {
        return result;
      }
      result = canSumTo(target - maxTerm * maxTerm, maxTerm - 1, termCount - 1) ||
        canSumTo(target, maxTerm - 1, termCount);
      cache[(target, maxTerm, termCount)] = result;
      return result;
    }
  }

  private static bool canUniquelySumTo(int target, int maxTerm, int termCount) {
    if (uniqueCache.TryGetValue((target, maxTerm, termCount), out bool result)) {
      return result;
    }
    result = canUniquelySumToImpl(target, maxTerm, termCount);
    uniqueCache[(target, maxTerm, termCount)] = result;
    return result;
  }

  private static bool canUniquelySumToImpl(int target, int maxTerm, int termCount) {
    if (termCount == 0) {
      return (target == 0);
    } else if (target < 0) {
      return false;
    } else {
      bool foundOne = false;
      for (int i = maxTerm; i >= 1; i--) {
        if (canSumTo(target - i * i, i - 1, termCount - 1)) {
          if (!canUniquelySumTo(target - i * i, i - 1, termCount - 1)) {
            // Not unique
            return false;
          }
          if (foundOne) {
            return false; // Duplicate solution
          } else {
            foundOne = true;
          }
        }
      }
      return foundOne;
    }
  }

}
