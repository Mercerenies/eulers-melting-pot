
#import <Foundation/Foundation.h>
#import <math.h>
#import <stdio.h>

double phi[1000001];

int main(int argc, const char* argv[]) {
  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  int n, i;
  for (i = 0; i <= 1000000; i++) {
    phi[i] = i;
  }
  for (i = 2; i <= 1000000; i++) {
    if (phi[i] == i) {
      n = i;
      while (n <= 1000000) {
        phi[n] = phi[n] * (1 - 1 / (double)i);
        n += i;
      }
    }
  }
  int max = 0;
  double maxratio = 0;
  for (n = 1; n <= 1000000; n++) {
    int val = phi[n];
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
