
#include <iostream>

using namespace std;

constexpr size_t SIZE = 99999;
unsigned char cache[SIZE] = {};

bool is_prime_impl(int n) {
    if (n < 2)
        return false;
    for (int i = 2; i <= n / 2; i++) {
        if (n % i == 0)
            return false;
    }
    return true;
}

bool is_prime(int n) {
    if (n < SIZE) {
        if (cache[(size_t)n] == 16)
            cache[(size_t)n] = (unsigned char)is_prime_impl(n);
        return cache[(size_t)n] != 0;
    } else {
        return is_prime_impl(n);
    }
}

bool distinct_primes_is(int n, int match) {
    int count = 0;
    for (int i = 2; i <= n; i++) {
        if ((n % i == 0) && (is_prime(i)))
            count++;
        if (count > match)
            return false;
    }
    return (count == match);
}

int main() {
    for (size_t i = 0; i < SIZE; i++)
        cache[i] = 16;
    int cycle_length = 4;
    int matches = 0;
    int result = -1;
    for (int i = 2; ; i++) {
        if (distinct_primes_is(i, cycle_length))
            matches++;
        else
            matches = 0;
        if (matches == cycle_length) {
            result = i - matches + 1;
            break;
        }
        if (i % 1000 == 0)
            std::cout << i << std::endl;
    }
    std::cout << result << std::endl;
    return 0;
}
