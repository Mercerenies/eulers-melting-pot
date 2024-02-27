
// Failed attempt to make problem181_1.cs faster. Slowed it down to 9
// seconds instead.

using System;

public class problem181_2 {

  private static int index(int white, int black, int lastWhite, int lastBlack) {
    return lastBlack + 61 * (lastWhite + 41 * (black + 61 * white));
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
            int currIndex = index(white, black, lastWhite, lastBlack);
            int whiteLimit = Math.Min(white, lastWhite);
            for (int whiteSpent = 0; whiteSpent <= whiteLimit; whiteSpent++) {
              int blackLimit = (whiteSpent == lastWhite ? Math.Min(black, lastBlack) : black);
              for (int blackSpent = 0; blackSpent <= blackLimit; blackSpent++) {
                int newWhite = white - whiteSpent;
                int newBlack = black - blackSpent;
                // Note: newWhite + 1 instead of newWhite, since we
                // need to be able to distinguish the cases where we
                // went "down" by a white from those where we haven't
                // (see the conditional for blackLimit above). But we
                // don't have to distinguish any further than that.
                int newIndex = index(newWhite, newBlack, Math.Min(whiteSpent, newWhite + 1), Math.Min(blackSpent, newBlack));
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
