
// Javagony :)

import java.util.*;
import java.io.*;

public class problem96 {

  public static final int[] BOX_OFFSETS = new int[] { 0, 1, 2, 9, 10, 11, 18, 19, 20 };

  private int totalSum;

  public problem96() {
    totalSum = 0;
  }

  // These are our if statements. They don't look much like if
  // statements, but hey.
  public class HaltProgramException extends RuntimeException {}
  public class PuzzleLoadedException extends RuntimeException {}
  public class LineLoadedException extends RuntimeException {}
  public class SuccessfullySolvedException extends RuntimeException {}
  public class GivenCellException extends RuntimeException {}
  public class EndFindValidException extends RuntimeException {}
  public class MustBacktrackException extends RuntimeException {}
  public class NotValidOptionException extends RuntimeException {}
  public class InvalidateOptionException extends RuntimeException {}

  public static void noop(int a) {
    // Need this to get around Java's statement/expression issues.
  }

  public static void throwIfTrue(boolean condition, RuntimeException exception) {
    try {
      noop(1 / Boolean.compare(condition, true));
    } catch (ArithmeticException ignore) {
      throw exception;
    }
  }

  public class ValidOptionsFinder {
    private int[] puzzle;
    private int idx;
    private boolean[] valid;

    public ValidOptionsFinder(int[] puzzle, int idx) {
      this.puzzle = puzzle;
      this.idx = idx;
      this.valid = new boolean[9];
      Arrays.fill(valid, true);
    }

    public boolean[] getValidOptions() {
      return valid;
    }

    private void _rowResolution(int col) {
      throwIfTrue(col >= 9, new EndFindValidException());
      int row = idx / 9;
      try {
        throwIfTrue(puzzle[row * 9 + col] > 0, new InvalidateOptionException());
      } catch (InvalidateOptionException e) {
        valid[ puzzle[row * 9 + col] - 1 ] = false;
      }
      _rowResolution(col + 1);
    }

    private void _colResolution(int row) {
      throwIfTrue(row >= 9, new EndFindValidException());
      int col = idx % 9;
      try {
        throwIfTrue(puzzle[row * 9 + col] > 0, new InvalidateOptionException());
      } catch (InvalidateOptionException e) {
        valid[ puzzle[row * 9 + col] - 1 ] = false;
      }
      _colResolution(row + 1);
    }

    private int getBox() {
      int rowbox = idx / 27;
      int colbox = (idx % 9) / 3;
      return rowbox * 27 + colbox * 3;
    }

    private void _boxResolution(int innerIdx) {
      throwIfTrue(innerIdx >= 9, new EndFindValidException());
      int box = getBox();
      int curr = box + BOX_OFFSETS[innerIdx];
      try {
        throwIfTrue(puzzle[curr] > 0, new InvalidateOptionException());
      } catch (InvalidateOptionException e) {
        valid[ puzzle[curr] - 1 ] = false;
      }
      _boxResolution(innerIdx + 1);
    }

    public void findOption() {
      try {
        _rowResolution(0);
      } catch (EndFindValidException ignore) {}
      try {
        _colResolution(0);
      } catch (EndFindValidException ignore) {}
      try {
        _boxResolution(0);
      } catch (EndFindValidException ignore) {}
    }

  }

  public class SinglePuzzleSolver {
    private int[] puzzle;

    public SinglePuzzleSolver() {
      puzzle = new int[81];
    }

    public int[] getPuzzle() {
      return puzzle;
    }

    private void _loadLine(int i, int j, String x) {
      throwIfTrue(j == 9, new LineLoadedException());
      puzzle[i * 9 + j] = (int)(x.charAt(j) - '0');
      _loadLine(i, j + 1, x);
    }

    private void _loadPuzzle(int i, BufferedReader reader) throws IOException {
      throwIfTrue(i == 9, new PuzzleLoadedException());
      String line = reader.readLine();
      try {
        _loadLine(i, 0, line);
      } catch (LineLoadedException e) {}
      _loadPuzzle(i + 1, reader);
    }

    public void loadPuzzle(BufferedReader reader) throws IOException {
      try {
        reader.readLine(); // Ignore "GRID XX" line
        _loadPuzzle(0, reader);
      } catch (PuzzleLoadedException ignore) {}
    }

    private void _trySolutions(int idx, boolean[] valid, int validIdx) {
      try {
        throwIfTrue(validIdx >= 9, new MustBacktrackException());
      } catch (MustBacktrackException e) {
        puzzle[idx] = 0;
        throw e;
      }

      try {
        throwIfTrue(!valid[validIdx], new NotValidOptionException());
      } catch (NotValidOptionException ignore) {
        _trySolutions(idx, valid, validIdx + 1);
      }

      puzzle[idx] = validIdx + 1;
      try {
        _solvePuzzle(idx + 1);
      } catch (MustBacktrackException ignore) {
        _trySolutions(idx, valid, validIdx + 1);
      }

    }

    private void _solvePuzzle(int idx) {
      throwIfTrue(idx >= 81, new SuccessfullySolvedException());
      try {
        throwIfTrue(puzzle[idx] > 0, new GivenCellException());
      } catch (GivenCellException ignore) {
        _solvePuzzle(idx + 1);
      }

      ValidOptionsFinder finder = new ValidOptionsFinder(puzzle, idx);
      finder.findOption();
      boolean[] valid = finder.getValidOptions();
      _trySolutions(idx, valid, 0);

    }

    public void solvePuzzle() {
      try {
        _solvePuzzle(0);
      } catch (SuccessfullySolvedException ignore) {}
    }

  }

  public int getTotalSum() {
    return totalSum;
  }

  public void solveAllPuzzles(int i, BufferedReader reader) throws IOException {
    throwIfTrue(i == 0, new HaltProgramException());

    SinglePuzzleSolver solver = new SinglePuzzleSolver();
    solver.loadPuzzle(reader);
    solver.solvePuzzle();
    int[] puzzle = solver.getPuzzle();
    totalSum += puzzle[0] * 100 + puzzle[1] * 10 + puzzle[2];

    solveAllPuzzles(i - 1, reader);
  }

  public static void main(String[] args) {
    problem96 solver = new problem96();
    try (BufferedReader reader = new BufferedReader(new FileReader("./files/p096_sudoku.txt"))) {
      solver.solveAllPuzzles(50, reader);
    } catch (Exception e) {}
    System.out.println(solver.getTotalSum());
  }

}
