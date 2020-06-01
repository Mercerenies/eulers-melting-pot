// -*- C -*- (Close enough for syntax highlighting purposes)

int[] p_values;

int pentagonal(int k) {
  return (k * (3 * k - 1)) / 2;
}

int p_value_cache(int n) {
  if (n < 0)
    return 0;
  else if (n == 0)
    return 1;
  else
    return p_values[n];
}

int gen_next() {
  int m = p_values.length;
  // Compute p(m)
  int total = 0;
  int k = 1;
  while (pentagonal(k) <= m) {
    int sgn = (k % 2 == 0) ? -1 : 1;
    total += sgn * (p_value_cache(m - pentagonal(k)) + p_value_cache(m - pentagonal(-k)));
    total = (total % 1000000 + 1000000) % 10000000;
    k++;
  }
  p_values += total;
  return total;
}

void main () {
  p_values = { 1 };
  while (gen_next() % 1000000 != 0);
  int result = p_values.length - 1;
  print(@"$result\n");
}
