
#import <Foundation/Foundation.h>
#import <math.h>

char primes[1000001];

int phi(int n) {
  if (primes[n]) {
    return n - 1;
  }
  double prod = n;
  int p;
  for (p = 2; p <= sqrt(n); p++) {
    if (primes[p]) {
      if (n % p == 0) {
        prod = prod * (1 - 1.0 / p);
        if (p * p != n && (primes[n / p])) {
          prod = prod * (1 - ((double)p) / n);
        }
      }
    }
  }
  return (int)prod;
}

int main(int argc, const char* argv[]) {
  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  primes[0] = 0;
  primes[1] = 0;
  int n, i;
  for (n = 2; n <= 1000000; n++) {
    primes[n] = 1;
  }
  for (i = 2; i <= 1000000; i++) {
    if (primes[i]) {
      n = i + i;
      while (n <= 1000000) {
        primes[n] = 0;
        n = n + i;
      }
    }
  }
  int max = 0;
  double maxratio = 0;
  for (n = 1; n <= 1000000; n++) {
    int val = phi(n);
    double ratio = ((double)n) / val;
    if (ratio > maxratio) {
      max = n;
      maxratio = ratio;
    }
  }
  NSString* result = [NSString stringWithFormat:@"%d", max];
  puts(result.UTF8String);
  [pool release];
  return 0;
}
