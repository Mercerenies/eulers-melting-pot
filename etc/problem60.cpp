
#include <utility>
#include <string>
#include <unordered_map>
#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

typedef pair<int, int> concat_pair;

constexpr size_t SIZE = 99999;
unsigned char cache[SIZE] = {};
vector<int> _nth_prime;

struct pair_hash_t {
    size_t operator()(concat_pair x) const {
        hash<int> h;
        return h(x.first) * 100000 + h(x.second);
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

int nth_prime(int n) {
    while (_nth_prime.size() <= (size_t)n) {
        _nth_prime.push_back(next_prime(_nth_prime[_nth_prime.size() - 1]));
    }
    return _nth_prime[n];
}

bool is_concat_prime(int n, int m) {
    if (n > m)
        swap(n, m);
    auto match = concat_cache.find({n, m});
    if (match == concat_cache.end()) {
        int n0 = nth_prime(n);
        int m0 = nth_prime(m);
        auto str1 = to_string(n0) + to_string(m0);
        auto str2 = to_string(m0) + to_string(n0);
        bool result = is_prime(stoi(str1)) && is_prime(stoi(str2));
        concat_cache[{n, m}] = result;
        return result;
    }
    return match->second;
}

constexpr int ARR_SIZE = 4;

struct collection {
    int elems[ARR_SIZE];
};

struct less_than_coll {
    bool operator()(const collection& a, const collection& b) const {
        int sa = 0;
        int sb = 0;
        for (int i = 0; i < ARR_SIZE; i++) {
            sa += nth_prime(a.elems[i]);
            sb += nth_prime(b.elems[i]);
        }
        int i = ARR_SIZE - 1;
        while ((sa == sb) && (i > 0)) {
            sa -= nth_prime(a.elems[i]);
            sb -= nth_prime(b.elems[i]);
            --i;
        }
        return sa > sb;
    }
};

template <typename T>
struct Heap {
    T value;
    Heap<T>* left;
    Heap<T>* right;
};

template <typename T, typename Compare>
Heap<T>* merge(Heap<T>* left, Heap<T>* right, Compare cmp) {
    if (left == nullptr)
        return right;
    if (right == nullptr)
        return left;
    if (cmp(left->value, right->value))
        return merge(right, left, cmp);
    auto ll = left->left;
    auto lr = left->right;
    left->right = ll;
    if (lr == nullptr) {
        left->left = right;
        return left;
    } else {
        auto result = merge(lr, right, cmp);
        left->left = result;
        return left;
    }
}

template <typename T, typename Compare>
Heap<T>* insert(Heap<T>* heap, const T& value, Compare cmp) {
    return merge(heap, new Heap<T> { value, nullptr, nullptr }, cmp);
}

template <typename T, typename Compare>
pair<T, Heap<T>*> remove(Heap<T>* heap, Compare cmp) {
    auto left = heap->left;
    auto right = heap->right;
    auto value = heap->value;
    delete heap;
    return { value, merge(left, right, cmp) };
}

template <typename T>
void cleanup_heap(Heap<T>* heap) {
    if (heap != nullptr) {
        cleanup_heap(heap->left);
        cleanup_heap(heap->right);
        delete heap;
    }
}

Heap<collection>* append_onto(Heap<collection>* frontier, collection curr, less_than_coll cmp) {
    for (int i = 0; i < ARR_SIZE; i++) {
        if ((i < ARR_SIZE - 1) && (curr.elems[i] + 1 >= curr.elems[i + 1]))
            continue;
        ++curr.elems[i];
        frontier = insert(frontier, curr, cmp);
        --curr.elems[i];
    }
    return frontier;
}

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

int main() {
    // Init
    less_than_coll cmp;
    collection start = {{0, 1, 2, 3}};
    for (size_t i = 0; i < SIZE; i++)
        cache[i] = 16;
    _nth_prime.reserve(SIZE);
    _nth_prime.push_back(3);
    Heap<collection>* frontier = insert<collection, less_than_coll>(nullptr, start, cmp);
    // Run
    auto prev = start;
    prev.elems[1] = 0; // Make it invalid to start with
    while (frontier != nullptr) {
        auto value = remove(frontier, cmp);
        // Dup
        if (std::equal(begin(prev.elems), end(prev.elems), begin(value.first.elems))) {
            frontier = value.second;
            continue;
        }
        // /Dup
        //cout << nth_prime(value.first.elems[0]) + nth_prime(value.first.elems[1]) + nth_prime(value.first.elems[2]) + nth_prime(value.first.elems[3]) << endl;
        frontier = append_onto(value.second, value.first, cmp);
        if (is_done(value.first)) {
            int sum = 0;
            for (int i = 0; i < ARR_SIZE; i++) {
                sum += nth_prime(value.first.elems[i]);
            }
            cout << sum << endl;
            break;
        }
        prev = value.first;
    }
    // Cleanup
    cleanup_heap(frontier);
    return 0;
}
