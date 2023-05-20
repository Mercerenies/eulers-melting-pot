
#define LIMIT 12

int count_digits(int* digits, int needle) {
  int result = 0;
  for (int i = 0; i < LIMIT; i++) {
    if (digits[i] == needle) {
      result += 1;
    }
  }
  return result;
}

long to_number(int* digits) {
  long number;
  for (int i = 0; i < LIMIT; i++) {
    number = number * 10 + digits[i];
  }
  return number;
}

void add(int* digits, int index) {
  if (digits[index] < 9) {
    digits[index] += 1;
  } else {
    digits[index] = 0;
    add(digits, index - 1);
  }
}

void sub(int* digits, int index) {
  if (digits[index] > 0) {
    digits[index] -= 1;
  } else {
    digits[index] = 9;
    sub(digits, index - 1);
  }
}

long run(int* digits, int index, int needle) {
  if (index >= LIMIT) {
    long input = to_number(digits);
    if (count_digits(digits, needle) == input) {
      return input;
    } else {
      return 0l;
    }
  }

  long acc = 0l;
  for (int k = 0; k <= 9; k++) {
    digits[index] = k;
    //long lower = to_number(digits[index])
  }
  return acc;
}

int main() {
  //int digits[
}
