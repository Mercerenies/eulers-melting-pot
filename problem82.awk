
BEGIN {
  line = 0;
  FS = ",";
  width = 80;
  height = 80;
}

{
  for (x = 0; x < NF; x++) {
    arr[x, line] = $(x+1);
  }
  line++;
}

END {

  for (y = 0; y < height; y++) {
    acc[0, y] = arr[0, y];
  }

  for (x = 1; x < width; x++) {
    for (y = 0; y < height; y++) {
      mincost = acc[x - 1, y] + arr[x, y];
      for (y0 = 0; y0 < height; y0++) {
        cost = acc[x - 1, y0]
        if (y < y0) {
          a = y;
          b = y0;
        } else {
          a = y0;
          b = y;
        }
        for (curr = a; curr <= b; curr++) {
          cost += arr[x, curr];
        }
        if (cost < mincost) {
          mincost = cost;
        }
      }
      acc[x, y] = mincost;
    }
  }

  final = acc[width - 1, 0]
  for (y = 0; y < height; y++) {
    if (acc[width - 1, y] < final) {
      final = acc[width - 1, y];
    }
  }
  print final;

}
