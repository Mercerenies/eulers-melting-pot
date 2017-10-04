
#include <vector>
#include <unordered_map>
#include <iostream>

using namespace std;

bool is_prime_impl(int value) {
  for (int i = 2; i <= value / 2; i++) {
    if (value % i == 0)
      return false;
  }
  return true;
}

bool is_prime(int value) {
  static unordered_map<int, bool> cache;
  if (cache.find(value) == cache.end())
    cache[value] = is_prime_impl(value);
  return cache[value];
}

int try_sequence(char* buffer, const vector<int>& repls) {
  ////
}

int try_replacements(char* buffer, int digits, int repls) {
  ////
}

int try_count(int digits) {
  char* buffer = new char[count + 1];
  buffer[count] = 0;
  for (int i = 3; i < digits; i++) {
    for (int j = 0; j < count; j++)
      buffer[j] = '0';
    int result = try_replacements(buffer, digits, i);
    if (result >= 0) {
      delete[] buffer;
      return result;
    }
  }
  delete[] buffer;
}

int main() {
  ////
}
