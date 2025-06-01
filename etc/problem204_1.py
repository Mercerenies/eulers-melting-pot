
PRIMES = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
          41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

LIMIT = int(1e9)


def enumerate_hamming_numbers(base: int, index: int) -> int:
    if base > LIMIT:
        return 0
    if index >= len(PRIMES):
        return 1
    total = 0
    while base <= LIMIT:
        total += enumerate_hamming_numbers(base, index + 1)
        base *= PRIMES[index]
    return total


if __name__ == "__main__":
    print(enumerate_hamming_numbers(base=1, index=0))

# Dreamberd notes:
# * Subtracting zero coerces to a number
# * Negative indexing is broken so we pad the array
# * I mean, it's Dreamberd; everything is broken
