
// Prohibitively slow on the online interpreter :(

phi (n) {
  extrn primes;
  if (primes[n]) {
    return(n - 1);
  }
  auto prod;
  auto p;
  prod = 1;
  p = 2;
  while (p < n) {
    if (primes[p]) {
      if (n % p == 0) {
        auto pow;
        auto base;
        pow = 0;
        base = p;
        while (n % base == 0) {
          pow++;
          base = base * p;
        }
        base = base / p;
        prod = prod * (base - base / p);
      }
    }
    p++;
  }
  return (prod);
}

main () {
  extrn primes;
  auto i;
  auto max;
  auto maxrat;
  i = 0;
  while (i < 1000000) {
    primes[i] = 1;
    i++;
  }
  primes[1] = 0;
  i = 2;
  while (i < 1000000) {
    if (primes[i] == 1) {
      auto n;
      n = i + i;
      while (n < 1000000) {
        primes[n] = 0;
        n = n + i;
      }
    }
    i++;
  }
  max = 0;
  maxrat = 0;
  i = 1;
  while (i < 1000000) {
    auto val;
    auto rat;
    val = phi(i);
    rat = i / val;
    if (rat > maxrat) {
      max = i;
      maxrat = rat;
    }
    i++;
  }
  if (max == 510510) {
    puts("Done");
  }
}

primes[1000000];