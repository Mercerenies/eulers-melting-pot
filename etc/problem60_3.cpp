
#include <array>
#include <vector>
#include <utility>
#include <sstream>
#include <iostream>
#include <numeric>
#include <unordered_map>

using TestPair = std::pair<long long, long long>;

std::vector<bool> primality;
std::vector<long long> primes;

std::unordered_map<long long, bool> cheap_primality;

bool is_prime_impl(long long n) {
  for (long long i = 2; i < n / 2; i++) {
    if (n % i == 0) {
      return false;
    }
  }
  return true;
}

void sieve(long long n) {
  primes.clear();
  primality = std::vector<bool>(n, true);
  primality[0] = false;
  primality[1] = false;
  for (long long i = 2; i < n; i++) {
    if (primality[i]) {
      primes.push_back(i);
      for (long long j = 2 * i; j < n; j += i)
        primality[j] = false;
    }
  }
}

bool is_prime(long long n) {
  if (n < primality.size()) {
    return primality[n];
  } else {
    auto ref = cheap_primality.find(n);
    if (ref == cheap_primality.end()) {
      bool result = is_prime_impl(n);
      cheap_primality[n] = result;
      return result;
    }
    return ref->second;
  }
}

bool is_valid(TestPair pair) {
  std::ostringstream str1, str2;
  str1 << pair.first << pair.second;
  if (!is_prime(atoll(str1.str().c_str())))
    return false;
  str2 << pair.second << pair.first;
  if (!is_prime(atoll(str2.str().c_str())))
    return false;
  return true;
}

template <long long N>
bool check(const std::array<long long, N>& input, long long upto) {
  for (long long i = 0; i < upto - 1; i++) {
    for (long long j = i + 1; j < upto; j++) {
      if (!is_valid({ primes[ input[i] ], primes[ input[j] ] })) {
        return false;
      }
    }
  }
  return true;
}

long long argmax = -1;

template <long long N>
long long seek_seq(std::array<long long, N>& input, long long start, long long bound) {
  if (!check<N>(input, start))
    return bound * N;
  if (start == N) { // Hooray!
    long long sum = 0;
    for (long long i : input)
      sum += primes[i];
    if (argmax == -1 || argmax > sum)
      argmax = sum;
    return sum;
  }
  long long ind0 = (start == 0 ? 0 : input[start - 1] + 1);
  long long sum = bound * N;
  for (long long i = ind0; primes[i] < bound; i++) {
    if ((argmax >= 0) && (primes[i] > argmax))
      continue;
    input[start] = i;
    long long curr = seek_seq<N>(input, start + 1, bound);
    if (curr < sum)
      sum = curr;
  }
  return sum;
}

int main() {
  constexpr long long SIZE = 5;
  std::array<long long, SIZE> curr;
  long long bound = 9999;
  long long result = bound * SIZE;

  while (result >= bound * SIZE) {
    sieve(std::max<long long>(bound * 10, 99999999));
    result = seek_seq<SIZE>(curr, 0, bound);
    bound *= 5;
  }
  std::cout << result << std::endl;
}
