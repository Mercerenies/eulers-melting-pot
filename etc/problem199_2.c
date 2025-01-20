
// Extremely low-level version of problem199.swift

#include <stdlib.h>
#include <stdio.h>

const int MAX_ARRAY_SIZE = 708588; // = 3 * 4 * 3^10
const double PI = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862;

const int TAYLOR_SERIES_TERMS = 10;

// https://math.stackexchange.com/a/732660/84460
double sqrt_approx(double x) {
  double sum = 1.0;
  for (int k = 0; k < TAYLOR_SERIES_TERMS; k++) {
    double product = 2.0 / (k + 1.0);
    for (int j = 1; j <= k; j++) {
      product *= (k + j) / j;
    }
    for (int j = 0; j < k + 1; j++) {
      product *= (- (x - 1.0) / 4.0);
    }
    printf("%f\n", product);
    sum -= product;
  }
  return sum;
}

double iterate(double* in, double* out, int input_count) {
  double area_covered = 0.0;
  for (int i = 0; i < input_count; i++) {
    double k1 = 1.0 / in[3 * i];
    double k2 = 1.0 / in[3 * i + 1];
    double k3 = 1.0 / in[3 * i + 2];
    double k4 = k1 + k2 + k3 + 2 * sqrt_approx(k1 * k2 + k2 * k3 + k3 * k1);
    area_covered += PI / (k4 * k4);
    out[9 * i] = 1.0 / k4;
    out[9 * i + 1] = in[3 * i];
    out[9 * i + 2] = in[3 * i + 1];
    out[9 * i + 3] = 1.0 / k4;
    out[9 * i + 4] = in[3 * i + 1];
    out[9 * i + 5] = in[3 * i + 2];
    out[9 * i + 6] = 1.0 / k4;
    out[9 * i + 7] = in[3 * i + 2];
    out[9 * i + 8] = in[3 * i];
  }
  return area_covered;
}

int main() {
  printf("%f\n", sqrt_approx(4.0));
  return 0;

  double outer_circle = -1.0;
  double initial_inner_circle = 0.46410161514; // = 2 sqrt(3) - 3
  double* gaps = malloc(MAX_ARRAY_SIZE * sizeof(double));

  double area_left = PI - initial_inner_circle * initial_inner_circle * PI * 3;

  // Calculate the center
  gaps[0] = initial_inner_circle;
  gaps[1] = initial_inner_circle;
  gaps[2] = initial_inner_circle;
  int elem_count = 1;
  for (int i = 0; i < 10; i++) {
    double* new_gaps = malloc(MAX_ARRAY_SIZE * sizeof(double));
    area_left -= iterate(gaps, new_gaps, elem_count);
    elem_count *= 3;
    free(gaps);
    gaps = new_gaps;
  }

  // Calculate the outer gaps
  double outer_area_covered = 0.0;
  gaps[0] = outer_circle;
  gaps[1] = initial_inner_circle;
  gaps[2] = initial_inner_circle;
  elem_count = 1;
  for (int i = 0; i < 10; i++) {
    double* new_gaps = malloc(MAX_ARRAY_SIZE * sizeof(double));
    outer_area_covered += iterate(gaps, new_gaps, elem_count);
    elem_count *= 3;
    free(gaps);
    gaps = new_gaps;
  }
  area_left -= 3 * outer_area_covered;

  printf("%.8f\n", area_left / PI);
  free(gaps);
  return 0;
}
