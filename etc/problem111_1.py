
def pow_mod(a, b, n):
    if b == 0:
        return 1
    elif b % 2 == 0:
        x = pow_mod(a, b // 2, n)
        return (x * x) % n
    else:
        return (pow_mod(a, b - 1, n) * a) % n

def miller_rabin_test(n, d, r, a):
    x = pow_mod(a, d, n)
    if x == 1 or x == n - 1:
        return True
    for _i in range(r):
        x = (x * x) % n
        if x == n - 1:
            return True
    return False

def is_prime(n):
    if n < 2:
        return False
    if n == 2 or n == 3 or n == 5 or n == 13 or n == 23 or n == 1662803:
        return True
    if n % 2 == 0 or n % 3 == 0 or n % 5 == 0:
        return False

    d = n - 1
    r = 0
    while d % 2 == 0:
        d //= 2
        r += 1

    # Wikipedia says these are sufficient for my input size
    if not miller_rabin_test(n, d, r, 2):
        return False
    if not miller_rabin_test(n, d, r, 13):
        return False
    if not miller_rabin_test(n, d, r, 23):
        return False
    if not miller_rabin_test(n, d, r, 1662803):
        return False
    return True

def _to_long(data):
    result = 0
    for x in data:
        result = 10 * result + x
    return result

def _generate(vec, digits, repeated, count, data, index):
    if count < 0:
        return
    if index >= digits:
        vec.append(_to_long(data))
        return
    if digits - index < count:
        return
    if digits - index == count:
        for i in range(index, digits):
            data[i] = repeated
        _generate(vec, digits, repeated, 0, data, digits)
        return
    for i in range(10):
        if i == 0 and index == 0:
            continue # No leading zeroes
        data[index] = i
        new_count = count - 1 if i == repeated else count
        _generate(vec, digits, repeated, new_count, data, index + 1)

def generate_with_digits(digits, repeated, count):
    result = []
    data = [0 for i in range(digits)]
    _generate(result, digits, repeated, count, data, 0)
    return result

def max_number_of_digits(digits, repeated):
    for i in range(digits, -1, -1):
        nums = generate_with_digits(digits, repeated, i)
        for num in nums:
            if is_prime(num):
                return i
    return 0

def sum_all(digits, repeated, count):
    total = 0
    nums = generate_with_digits(digits, repeated, count)
    for num in nums:
        if is_prime(num):
            total += num
    return total

digits = 10

sum_total = 0
for i in range(10):
    count = max_number_of_digits(digits, i)
    sum_total += sum_all(digits, i, count)
print(sum_total)
