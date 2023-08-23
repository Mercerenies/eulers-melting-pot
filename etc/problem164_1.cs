
// Same as problem164.el, but written using an explicit dynamic
// programming array so we have some direction to our computation.
//
// We only need to keep the (n-1) case to compute the n case, so data
// for the (n-1) case is stored in a 100-element array with the (a, b)
// value stored as a*10 + b.

using System;

public class problem164_1 {

  private static long[] iterate(long[] lastRow) {
    long[] newRow = new long[100];
    for (int i = 0; i < 100; i++) {
      int a = i / 10;
      int b = i % 10;
      int maxC = 9 - a - b;
      long sum = 0L;
      for (int c = 0; c <= maxC; c++) {
        sum += lastRow[b * 10 + c];
      }
      newRow[i] = sum;
    }
    return newRow;
  }

  public static void Main(String[] args) {
    long[] array = new long[100];
    Array.Fill(array, 1);

    for (int i = 0; i < 19; i++) {
      array = iterate(array);
    }

    long finalSum = 0L;
    for (int i = 1; i <= 9; i++) {
      finalSum += array[i];
    }
    Console.WriteLine(finalSum);
  }

}
