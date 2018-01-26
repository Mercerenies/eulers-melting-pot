
int gcd(int n, int m) {
  while (m != 0) {
    int t = m;
    m = n % m;
    n = t;
  }
  return n;
}

array(int) iterate(int n) {
  int num = 2;
  int den = 1;
  for (int i = 0; i < n; i++) {
    // Reciprocate
    int temp = num;
    num = den;
    den = temp;
    // Add two
    num += den * 2;
  }
  // Subtract one
  num -= den;
  // Reduce
  int div = gcd(num, den);
  num /= div;
  den /= div;
  return ({num, den});
}

int main() {
  int count = 0;
  for (int i = 0; i <= 1000; i++) {
    array(int) result = iterate(i);
    if (sizeof( (string) result[0] ) > sizeof( (string) result[1] )) {
      count += 1;
    }
  }
  write("%d\n", count);
}
