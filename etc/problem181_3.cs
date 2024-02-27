
// Same as problem181_1.cs, but using a different ordering on the
// tuples. Specifically, instead of keeping track of (w, b), we'll
// keep track of (w + b, b). This has the advantage that the natural
// ordering on tuples is preserved pointwise. That is, the following
// two properties hold.
//
// * w < w' implies (w + b, b) < (w' + b, b), and
//
// * b < b' implies (w + b, b) < (w + b', b').
//
// Never mind, go look at problem181_2.cs. It's simpler, and I haven't
// ironed out the details of this one so this one is incorrect right
// now.

using System;

public class problem181_3 {

  private static int index(int white, int black, int lastWhite, int lastBlack) {
    return lastBlack + 61 * (lastWhite + 41 * (black + 61 * white));
  }

  private static bool checkOrderingLessOrEqual(int w1, int b1, int w2, int b2) {
    if (w1 + b1 > w2 + b2) {
      return false;
    } else if (w1 + b1 == w2 + b2) {
      return (b1 <= b2);
    } else {
      return true;
    }
  }

  public static void Main(String[] args) {
    long[] array = new long[41 * 61 * 41 * 61];
    Array.Fill(array, 0);

    // Fill all positions with white == black == 0 with ones.
    for (int lastWhite = 0; lastWhite <= 40; lastWhite++) {
      for (int lastBlack = 0; lastBlack <= 60; lastBlack++) {
        array[index(0, 0, lastWhite, lastBlack)] = 1;
      }
    }

    for (int white = 0; white <= 40; white++) {
      for (int black = 0; black <= 60; black++) {
        if ((white == 0) && (black == 0)) {
          continue;
        }
        for (int lastWhite = 0; lastWhite <= 40; lastWhite++) {
          for (int lastBlack = 0; lastBlack <= 60; lastBlack++) {
            // Invariant: (whiteSpent + blackSpent, blackSpent) <= (lastWhite + lastBlack, blackSpent)
            int currIndex = index(white, black, lastWhite, lastBlack);
            int whiteLimit = Math.Min(white, lastWhite + lastBlack);
            for (int whiteSpent = 0; whiteSpent <= whiteLimit; whiteSpent++) {
              for (int blackSpent = 0; blackSpent <= black && checkOrderingLessOrEqual(whiteSpent, blackSpent, lastWhite, lastBlack); blackSpent++) {
                int newWhite = white - whiteSpent;
                int newBlack = black - blackSpent;
                int newIndex = index(newWhite, newBlack, Math.Min(newWhite, whiteSpent), Math.Min(newBlack, blackSpent));
                array[currIndex] += array[newIndex];
              }
            }
          }
        }
      }
    }
    Console.WriteLine(array[index(40, 60, 40, 60)]);
  }

}
