
#include <array>
#include <vector>
#include <unordered_map>
#include <utility>
#include <sstream>
#include <iostream>

using TestPair = std::pair<int, int>;

struct PairHash {
  std::size_t operator()(const TestPair& value) const {
    static std::hash<int> hash;
    return hash(value.first) ^ hash(value.second);
  }
};

std::vector<bool> primality;
std::vector<int> primes;
std::unordered_map<TestPair, bool, PairHash> cache;

bool is_prime_impl(int n) {
  if (n % 2 == 0 || n % 5 == 0)
    return false;
  for (int i = 0; primes[i] < n / 2; i++) {
    if (n % primes[i] == 0) {
      return false;
    }
  }
  return true;
}

void store_primes(int n) {
  for (int i = primality.size(); i <= n; i++) {
    bool val = is_prime_impl(i);
    primality.push_back(val);
    if (val)
      primes.push_back(i);
  }
}

bool is_prime(int n) {
  if (n >= primality.size())
    store_primes(n);
  return primality[n];
}

bool is_valid_impl(TestPair pair) {
  std::ostringstream str1, str2;
  str1 << pair.first << pair.second;
  if (!is_prime(atoi(str1.str().c_str())))
    return false;
  str2 << pair.second << pair.first;
  if (!is_prime(atoi(str2.str().c_str())))
    return false;
  return true;
}

bool is_valid(TestPair pair) {
  auto res = cache.find(pair);
  if (res == cache.end()) {
    bool result = is_valid_impl(pair);
    cache[pair] = result;
    return result;
  } else {
    return res->second;
  }
}

void init_primes() {
  // A valid solution can't contain either 2 or 5, so let's not even consider those
  primality.push_back(false); // 0
  primality.push_back(false); // 1
  primality.push_back(false); // 2
  primality.push_back(true ); // 3
  primality.push_back(false); // 4
  primality.push_back(false); // 5
  primes.push_back(3);
}

template <int N>
bool check(const std::array<int, N>& input) {
  for (int i = 0; i < N - 1; i++) {
    for (int j = i + 1; j < N; j++) {
      if (!is_valid({ primes[ input[i] ], primes[ input[j] ] })) {
        return false;
      }
    }
  }
  return true;
}

template <int N>
void print_array(const std::array<int, N>& input) {
  for (int x : input) {
    std::cout << primes[ x ] << " ";
  }
  std::cout << std::endl;
}

template <int N>
bool try_increment_once(int index, std::array<int, N>& input, int& sum, int upper_limit) {
  if (index <= 1)
    std::cout << sum << std::endl;
//    print_array<N>(input);
  sum -= primes[ input[index] ];
  ++input[index];
  sum += primes[ input[index] ];
  for (int i = index + 1; i < N; i++) {
    sum -= primes[ input[i] ];
    input[i] = input[i - 1] + 1;
    sum += primes[ input[i] ];
  }
  if (sum <= upper_limit) {
    return true;
  } else if (index == 0) {
    return false;
  } else {
    return try_increment_once<N>(index - 1, input, sum, upper_limit);
  }
}

template <int N>
bool increment(std::array<int, N>& input, int& sum, int lower_limit, int upper_limit) {
  const int index = N - 1;
  do {
    if (!try_increment_once<N>(index, input, sum, upper_limit))
      return false;
  } while (sum < lower_limit);
  return true;
}

int main() {
  constexpr int SIZE = 5;
  init_primes();
  int lower_limit = 0;
  int upper_limit = 792;
  int solution = -1;
  int sum = 0;

  store_primes(999999);
  while (solution < 0) {
    std::cout << "START" << std::endl;

    std::array<int, SIZE> input;
    for (int i = 0; i < SIZE; i++) {
      input[i] = i;
    }
    input[SIZE - 1]--; // So the first increment will "undo" this effect
    sum = 0;
    for (int i = 0; i < SIZE; i++) {
      sum += primes[ input[i] ];
    }

    lower_limit = upper_limit;
    upper_limit *= 10;
    store_primes(upper_limit);
    while (increment<SIZE>(input, sum, lower_limit, upper_limit)) {
      //print_array<SIZE>(input);
      if (check<SIZE>(input)) {
        std::cout << "Found match " << sum << std::endl;
        solution = sum;
        upper_limit = sum - 1;
      }
    }

  }
  std::cout << solution << std::endl;

  return 0;
}
