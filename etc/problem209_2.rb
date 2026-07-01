
# problem209_1.rb but everything is inlined :D

FIBO = [0] * 64
FIBO[0] = 1
FIBO[1] = 2
(2..63).each do |i|
  FIBO[i] = FIBO[i - 1] + FIBO[i - 2]
end

total = 1
64.times do |i|
  cycle_length = 0
  k = i
  loop do
    cycle_length += 1

    a = (k >> 5) & 1
    b = (k >> 4) & 1
    c = (k >> 3) & 1
    k = (k & 0b011111) * 2 + (a ^ (b & c))

    if k > i
      cycle_length = -1
      break
    end
    break if k == i
  end

  if cycle_length > 0
    if cycle_length == 2
      total *= 3
    elsif cycle_length > 2
      total *= (FIBO[cycle_length - 1] + FIBO[cycle_length - 3])
    end
  end
end

puts total
