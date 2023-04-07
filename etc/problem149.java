
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
        throw new IndexOutOfBoundsException();
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
    for (int r = 0; r < GRID_SIZE; r++) {
      if (r % 10 == 0) {
        System.out.println(r);
      }
      for (int c = 0; c < GRID_SIZE; c++) {
        best = Math.max(best, maxSumFrom(grid, r, c));
      }
    }
    System.out.println(best);
  }

  public static long maxSumFrom(Grid grid, int row, int column) {
    if (grid.get(row, column) < 0L) {
      // It's never in our advantage to start from a negative number,
      // since we could just omit the negative and still get a good answer.
      return 0L;
    }

    return Math.max(
      Math.max(
        rightSum(grid, row, column),
        downSum(grid, row, column)
      ),
      Math.max(
        diaSum(grid, row, column),
        antiDiaSum(grid, row, column)
      )
    );
  }

  private static long rightSum(Grid grid, int row, int column) {
    long acc = 0L;
    long best = 0L;
    for (int i = 0; i < GRID_SIZE - column; i++) {
      acc += grid.get(row, column + i);
      best = Math.max(acc, best);
    }
    return best;
  }

  private static long downSum(Grid grid, int row, int column) {
    long acc = 0L;
    long best = 0L;
    for (int i = 0; i < GRID_SIZE - row; i++) {
      acc += grid.get(row + i, column);
      best = Math.max(acc, best);
    }
    return best;
  }

  private static long diaSum(Grid grid, int row, int column) {
    long acc = 0L;
    long best = 0L;
    for (int i = 0; i < Math.min(GRID_SIZE - row, GRID_SIZE - column); i++) {
      acc += grid.get(row + i, column + i);
      best = Math.max(acc, best);
    }
    return best;
  }

  private static long antiDiaSum(Grid grid, int row, int column) {
    long acc = 0L;
    long best = 0L;
    for (int i = 0; i < Math.min(GRID_SIZE - row, column); i++) {
      acc += grid.get(row + i, column - i);
      best = Math.max(acc, best);
    }
    return best;
  }

}
