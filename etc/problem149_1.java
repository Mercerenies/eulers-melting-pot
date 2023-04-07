
public class problem149 {

  public static final int GRID_SIZE = 2000;

  public static class Grid {
    private static long[] impl;

    public Grid() {
      impl = new long[4000000];
      initGrid();
    }

    public boolean inBounds(int row, int column) {
      return ((row >= 0) && (row < GRID_SIZE) && (column >= 0) && (column < GRID_SIZE));
    }

    public long get(int row, int column) {
      if (!inBounds(row, column)) {
        return 0;
      }
      return impl[row * GRID_SIZE + column];
    }

    private void initGrid() {
      for (int k = 1; k <= 4000000; k++) {
        impl[k - 1] = s(k);
      }
    }

    private long s(int k) {
      // Intermediate computation may exceed the limits of int, even
      // though the final result is always an int.
      if (k <= 55L) {
        return ((100003L - 200003L * k + 300007L * k * k * k) % 1000000L) - 500000L;
      } else {
        long sk24 = impl[k - 25]; // impl is 0-based
        long sk55 = impl[k - 56]; // impl is 0-based
        return ((sk24 + sk55 + 1000000L) % 1000000L) - 500000L;
      }
    }

  }

  public static void main(String[] args) {
    Grid grid = new Grid();
    long best = 0L;

    // Row sums
    for (int r = 0; r < GRID_SIZE; r++) {
      long localSum = 0L;
      for (int i = 0; i < GRID_SIZE; i++) {
        long value = grid.get(r, i);
        localSum = Math.max(localSum + value, value);
        best = Math.max(localSum, best);
      }
    }

    // Column sums
    for (int c = 0; c < GRID_SIZE; c++) {
      long localSum = 0L;
      for (int i = 0; i < GRID_SIZE; i++) {
        long value = grid.get(i, c);
        localSum = Math.max(localSum + value, value);
        best = Math.max(localSum, best);
      }
    }

    // Diagonal sums
    for (int x = - GRID_SIZE; x < GRID_SIZE; x++) {
      long localSum = 0L;
      for (int i = 0; i < GRID_SIZE; i++) {
        long value = grid.get(x + i, i);
        localSum = Math.max(localSum + value, value);
        best = Math.max(localSum, best);
      }
    }

    // Antidiagonal sums
    for (int x = 0; x < 2 * GRID_SIZE; x++) {
      long localSum = 0L;
      for (int i = 0; i < GRID_SIZE; i++) {
        long value = grid.get(x - i, i);
        localSum = Math.max(localSum + value, value);
        best = Math.max(localSum, best);
      }
    }

    System.out.println(best);
  }

}
