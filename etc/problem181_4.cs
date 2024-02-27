
// Okay, I was overthinking this a lot. This algorithm was originally
// written in C by Robert_Gerbicz on the Project Euler forum.
// Translated to C# here so I can test it.
//
// <100ms in C#. Runs perfectly.

using System;

public class problem181_4 {

  public static void Main(String[] args) {
    long[] array = new long[41 * 61];
    Array.Fill(array, 0);
    array[0] = 1;
    for (int whiteToSpend = 0; whiteToSpend <= 40; whiteToSpend++) {
      for (int blackToSpend = 0; blackToSpend <= 60; blackToSpend++) {
        if ((whiteToSpend == 0) && (blackToSpend == 0)) {
          continue;
        }
        for (int whiteLeft = whiteToSpend; whiteLeft <= 40; whiteLeft++) {
          for (int blackLeft = blackToSpend; blackLeft <= 60; blackLeft++) {
            int currIndex = whiteLeft * 61 + blackLeft;
            int newIndex = (whiteLeft - whiteToSpend) * 61 + (blackLeft - blackToSpend);
            array[currIndex] += array[newIndex];
          }
        }
      }
    }
    Console.WriteLine(array[41 * 61 - 1]);
  }

}
