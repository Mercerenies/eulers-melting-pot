
#include <utility>
#include <string>
#include <unordered_map>
#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

typedef pair<int, int> concat_pair;

constexpr size_t SIZE = 999999;
unsigned char cache[SIZE] = {};

struct pair_hash_t {
    size_t operator()(concat_pair x) const {
        hash<int> h;
        return h(x.first) * 1000000 + h(x.second);
    }
};

unordered_map<concat_pair, bool, pair_hash_t> concat_cache;

bool is_prime_impl(int n) {
    // In this universe, 2 and 5 are not prime numbers >.<
    if ((n <= 2) || (n == 5))
        return false;
    for (int i = 2; i <= n / 2; i++) {
        if (n % i == 0)
            return false;
    }
    return true;
}

bool is_prime(int n) {
    if ((size_t)n < SIZE) {
        if (cache[(size_t)n] == 16) {
            cache[(size_t)n] = (unsigned char)is_prime_impl(n);
        }
        return cache[(size_t)n] != 0;
    } else {
        return is_prime_impl(n);
    }
}

int next_prime(int x) {
    while (!is_prime(++x));
    return x;
}

bool is_concat_prime(int n, int m) {
    if (n > m)
        swap(n, m);
    auto match = concat_cache.find({n, m});
    if (match == concat_cache.end()) {
        auto str1 = to_string(n) + to_string(m);
        auto str2 = to_string(m) + to_string(n);
        bool result = is_prime(stoi(str1)) && is_prime(stoi(str2));
        concat_cache[{n, m}] = result;
        return result;
    }
    return match->second;
}

constexpr int ARR_SIZE = 4;
constexpr int SUM_BOUND = 30000;

struct collection {
    int elems[ARR_SIZE];
};

bool is_done(const collection& x) {
    for (int i = 0; i < ARR_SIZE; i++) {
        for (int j = i + 1; j < ARR_SIZE; j++) {
            if (!is_concat_prime(x.elems[i], x.elems[j]))
                return false;
        }
    }
    // Hooray!
    return true;
}

void next(collection& coll) {

}

int main() {
    // Init
    collection start = {{3, 7, 11, 13}};
    for (size_t i = 0; i < SIZE; i++)
        cache[i] = 16;
    // Run
    
    // Cleanup
    cleanup_heap(frontier);
    return 0;
}
