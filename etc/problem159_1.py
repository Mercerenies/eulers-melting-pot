
# Smarter dynamic-programming solution that computes everything
# up-front. 3 seconds.

LIMIT = 1000000

# Digital root sum is just modulo 9, with the exception that numbers divisible by nine produce 9, not 0. See https://en.wikipedia.org/wiki/Digital_root
def digital_sum(x):
    return (x % 9) or 9

all_digit_root_sums = [0] * LIMIT

for i in range(2, LIMIT):
    all_digit_root_sums[i] = max(all_digit_root_sums[i], digital_sum(i))
    j = i + i
    n = 2
    while j < LIMIT and n <= i:
        all_digit_root_sums[j] = max(all_digit_root_sums[j], all_digit_root_sums[i] + all_digit_root_sums[n])
        j += i
        n += 1

s = 0
for i in range(2, LIMIT):
    s += all_digit_root_sums[i]
print(s)
