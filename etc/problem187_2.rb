
# Identical to problem187.c but translated into Ruby. I need to see if
# naive Ruby is fast enough to run this, before trying to do it in the
# Minus interpreter written in Ruby.
#
# 1m23s, not great.

LIMIT = 100_000_000

def sieve_of_eratosthenes(limit)
  result = Array.new(limit, true)
  (2...limit).each do |i|
    if result[i]
      j = i + i
      while j < limit
        result[j] = false
        j += i
      end
    end
  end
  result
end

primes_bitmask = sieve_of_eratosthenes(LIMIT)
primes = (2...LIMIT).select { |i| primes_bitmask[i] }

final_count = 0
primes.each_with_index do |pi, i|
  break if pi * pi >= LIMIT
  (i...primes.size).each do |j|
    pj = primes[j]
    break if pi * pj >= LIMIT
    final_count += 1
  end
end
p final_count
