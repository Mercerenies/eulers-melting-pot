
# Same as problem131_1.rb but using naive trial division for primality checking

def prime?(n)
  return false if n < 2
  (2..n/2).each do |x|
    return false if n % x == 0
  end
  true
end

count = 0
k = 0
loop do
  p = 3 * k * k + 3 * k + 1
  break if p >= 1000000
  count += 1 if prime? p
  k += 1
end
puts count
