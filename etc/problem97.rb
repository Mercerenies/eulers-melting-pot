
Max = 10 ** 10

def pow2(n)
  result = 1
  (1..n).each do
    result = (result * 2) % Max
  end
  result
end

puts((28433 * pow2(7830457) + 1) % Max)
