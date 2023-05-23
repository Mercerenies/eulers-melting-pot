
# Trying to do tricks in count_for to make this viable in Burlesque.

number_of_factors = (x) ->
  count = 0
  i = 1
  while i * i <= x
    if x % i == 0
      if i * i == x
        count += 1
      else
        count += 2
    i += 1
  count

count_for = (ten_n, a, b) ->
  p = ten_n * (a + b) / (a * b)
  # Remove all powers of 5 and 2 and calculate the factor count of
  # those parts directly. Then factor count what's left.
  fives = 0
  while p % 5 == 0
    p /= 5
    fives += 1
  twos = 0
  while p % 2 == 0
    p /= 2
    twos += 1
  number_of_factors(p) * (fives + 1) * (twos + 1)

count_solutions = (n) ->
  count = 0
  ten_n = 10 ** n

  # Let a = 1, b is a power of 2 times a power of 5.
  t = 1
  for i in [0..n]
    f = 1
    for j in [0..n]
      count += count_for(ten_n, 1, t * f)
      f *= 5
    t *= 2

  # Now let a be a nontrivial power of 2 and b a nontrivial power of
  # 5.
  a = 2
  for i in [1..n]
    b = 5
    for j in [1..n]
      count += count_for(ten_n, a, b) if a <= b
      b *= 5
    a *= 2

  # Now let b be a nontrivial power of 2 and a a nontrivial power of
  # 5.
  a = 5
  for i in [1..n]
    b = 2
    for j in [1..n]
      count += count_for(ten_n, a, b) if a <= b
      b *= 2
    a *= 5

  count

sum = (arr) ->
  arr.reduce ((x, y) -> x + y), 0

console.log(sum(count_solutions(i) for i in [1..9]))
