
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Digit-by-digit_calculation

def digits_of_sqrt(square, count)
  digit_sum = 0
  c = square
  p = 0
  (1..count).each do
    x = (9..0).step(-1).find { |x| x * (20 * p + x) <= c }
    y = x * (20 * p + x)
    digit_sum += x
    p = p * 10 + x
    c = 100 * (c - y)
  end
  digit_sum
end

perf = 1

final_sum = 0
(1..100).each do |i|
  if perf * perf == i
    perf += 1
    next
  end
  final_sum += digits_of_sqrt(i, 100)
end
puts final_sum

=begin

TOP OF STACK
c'
digit_sum'
x
j
p
i
final_sum
perf

BOTTOM_OF_STACK

(vvv INNER LOOP START vvv)

TOP OF STACK

j
p
c
digit_sum
i
final_sum
perf

BOTTOM_OF_STACK

(vvv LOOP START vvv)

TOP OF STACK

i
final_sum
perf

BOTTOM_OF_STACK

=end
