
import java.io.*;
import java.util.ArrayList;

public class problem81 {

  private static class Grid {
    private ArrayList< int[] > data;

    public Grid(ArrayList< int[] > data) {
      this.data = data;
    }

    int get(int x, int y) {
      return data.get(y)[x];
    }

    void put(int x, int y, int value) {
      data.get(y)[x] = value;
    }

  }

  public static final int WIDTH = 80;
  public static final int HEIGHT = 80;

  public static void main(String[] args) throws IOException {
    BufferedReader in = new BufferedReader(new FileReader("./files/p081_matrix.txt"));

    ArrayList<String> lines = new ArrayList<String>();
    while (true) {
      String curr = in.readLine();
      if (curr == null)
        break;
      lines.add(curr);
    }
    ArrayList< int[] > data = new ArrayList< int[] >();
    for (String line : lines) {
      String[] curr = line.split(",");
      int[] arr = new int[curr.length];
      for (int i = 0; i < curr.length; i++) {
        arr[i] = Integer.parseInt(curr[i]);
      }
      data.add(arr);
    }
    Grid grid = new Grid(data);

    // Dynamic programming! :D
    for (int x = 0; x < WIDTH; x++) {
      for (int y = 0; y < HEIGHT; y++) {
        if ((x == 0) && (y == 0)) {
          // First iteration; nothing to be done
        } else if (x == 0) {
          grid.put(x, y, grid.get(x, y) + grid.get(x, y - 1));
        } else if (y == 0) {
          grid.put(x, y, grid.get(x, y) + grid.get(x - 1, y));
        } else {
          int left = grid.get(x - 1, y);
          int up = grid.get(x, y - 1);
          grid.put(x, y, grid.get(x, y) + Math.min(left, up));
        }
      }
    }
    System.out.println(grid.get(WIDTH - 1, HEIGHT - 1));
  }

}
