
hits = Hash.new(0)

MaxLen = 1_500_000

# Using Euclid's formula to generate all Pythagorean triples
# (https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple)

m = 1
loop do
  1.upto(m - 1).each do |n|
    next if n % 2 == 1 and m % 2 == 1
    next if n.gcd(m) != 1
    k = 1
    loop do
      a = k * (m * m - n * n)
      b = k * (2 * m * n)
      c = k * (m * m + n * n)
      break if a + b + c > MaxLen
      hits[a + b + c] += 1
      k += 1
    end
    break if m * m + n * n > MaxLen or 2 * m * n > MaxLen
  end
  m += 1
  break if m * m > MaxLen
end

puts hits.count { |_, v| v == 1 }
