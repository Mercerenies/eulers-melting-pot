
# Problem 209 from problem209.py but simplified down to only the
# strictly-needed bits.

def permute(n)
  a = (n >> 5) & 1
  b = (n >> 4) & 1
  c = (n >> 3) & 1
  (n & 0b011111) * 2 + (a ^ (b & c))
end

def fibo(n)
  a = 1
  b = 2
  n.times do
    a, b = b, a + b
  end
  a
end

def count_for_component(cycle_length)
  if cycle_length < 3
    [1, 1, 3][cycle_length]
  else
    fibo(cycle_length - 1) + fibo(cycle_length - 3)
  end
end

def get_cycle_length(i)
  # We define the cycle length to be -1 if this is NOT the smallest
  # number in the cycle (to ensure that we count each cycle exactly
  # once).
  cycle_length = 0
  k = i
  loop do
    cycle_length += 1
    k = permute(k)
    return -1 if k > i
    return cycle_length if k == i
  end
end

total = 1
64.times do |i|
  cycle_length = get_cycle_length(i)
  if cycle_length > 0
    total *= count_for_component(cycle_length)
  end
end

puts total
