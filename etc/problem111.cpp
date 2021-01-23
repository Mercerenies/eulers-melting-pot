
#include <vector>
#include <iostream>

using namespace std;

// ......... 64-bit unsigned is not big enough .......

/*
std::vector<long long> primes;
*/
bool _is_prime(long long n) {
  if (n < 2)
    return false;
  for (long long i = 2ll; i <= n / 2ll; i++) {
    if (n % i == 0ll)
      return false;
  }
  return true;
}

unsigned long long modulo(unsigned long long a, unsigned long long b) {
  return (a % b + b) % b;
}

unsigned long long pow_mod(unsigned long long a, unsigned long long b, unsigned long long x) {
  if (b == 0ll) {
    return 1ll;
  } else if (b % 2 == 0) {
    unsigned long long intermediate = pow_mod(a, b / 2, x);
    return modulo(intermediate * intermediate, x);
  } else {
    return modulo(pow_mod(a, b - 1, x) * a, x);
  }
}

bool do_miller_rabin_test(long long n, long long d, long long r, long long a) {
  unsigned long long x = pow_mod(a, d, n);
  if ((x == 1) || (x == n - 1))
      return true;
  for (long long i = 0ll; i < r - 1; i++) {
    x = modulo(x * x, n);
    if (x == n - 1)
      return true;
  }
  return false;
}

bool is_prime(long long n) {
  if (n < 2)
    return false;
  if ((n == 2) || (n == 3) || (n == 5) || (n == 13) || (n == 23) || (n == 1662803))
    return true;
  if ((n % 2 == 0) || (n % 3 == 0) || (n % 5 == 0))
    return false;

  long long d = n - 1ll;
  long long r = 0ll;
  while (d % 2 == 0) {
    d /= 2ll;
    r += 1ll;
  }

  // Wikipedia says these are sufficient for my input size.
  if (!do_miller_rabin_test(n, d, r, 2))
    return false;
  if (!do_miller_rabin_test(n, d, r, 13))
    return false;
  if (!do_miller_rabin_test(n, d, r, 23))
    return false;
  if (!do_miller_rabin_test(n, d, r, 1662803))
    return false;
  return true;
}

long long _to_long(int* data, int length) {
  long long result = 0ll;
  for (int i = 0; i < length; i++) {
    result = 10ll * result + data[i];
  }
  return result;
}

void _generate(std::vector<long long>& vec, int digits, int repeated, int count, int* data, int index) {
  if (count < 0)
    return;
  if (index >= digits) {
    vec.push_back(_to_long(data, digits));
    return;
  }

  if (digits - index < count)
    return;
  if (digits - index == count) {
    for (int i = index; i < digits; i++)
      data[i] = repeated;
    _generate(vec, digits, repeated, 0, data, digits);
    return;
  }

  for (int i = 0; i < 10; i++) {
    if ((i == 0) && (index == 0))
      continue; // No leading zeroes
    data[index] = i;
    int new_count = (i == repeated) ? count - 1 : count;
    _generate(vec, digits, repeated, new_count, data, index + 1);
  }

}

std::vector<long long> generate_with_digits(int digits, int repeated, int count) {
  std::vector<long long> result;
  std::vector<int> data;
  for (int i = 0; i < digits; i++) {
    data.push_back(0);
  }
  _generate(result, digits, repeated, count, &data[0], 0);
  return result;
}

int max_number_of_digits(int digits, int repeated) {
  for (int i = digits; i >= 0; i--) {
    auto nums = generate_with_digits(digits, repeated, i);
    for (auto num : nums) {
      if (is_prime(num))
        return i;
    }
  }
  return 0;
}

long long sum_all(int digits, int repeated, int count) {
  long long sum = 0ll;
  auto nums = generate_with_digits(digits, repeated, count);
  for (auto num : nums) {
    if (is_prime(num))
      sum += num;
  }
  return sum;
}

/*
void sieve(long long max) {
  bool* sieve = new bool[max];
  for (long long i = 0ll; i < max; i++) {
    sieve[i] = true;
  }
  sieve[0] = false;
  sieve[1] = false;
  for (long long i = 2ll; i < max; i++) {
    if (sieve[i]) {
      primes.push_back(i);
      for (long long j = i * 2ll; j < max; j += i) {
        sieve[j] = false;
      }
    }
  }
  delete[] sieve;
}
*/

/*
void generate_primes(long long max) {
  for (long long i = 2ll; i < max; i++) {
    if (is_prime(i))
      primes.push_back(i);
    if (i % 10000ll == 0ll)
      std::cout << i << std::endl;
  }
}
*/

int main() {
  int digits = 10;

  long long sum_total = 0ll;
  for (int i = 0; i < 10; i++) {
    int count = max_number_of_digits(digits, i);
    sum_total += sum_all(digits, i, count);
    std::cout << i << std::endl;
  }
  std::cout << sum_total << std::endl;

  return 0;
}
