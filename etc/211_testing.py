

def sum_of_squares_of_divisors(n):
    s = 0
    for i in range(1, n + 1):
        if n % i == 0:
            s += i * i
    return s

print(sum_of_squares_of_divisors(1205400))  # 3553225000000
