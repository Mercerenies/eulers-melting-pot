
# Working I believe but prohibitively slow

$upperlimit = 1000001

$primes = Array.new($upperlimit, true)
$primes[0] = false
$primes[1] = false

(2..$upperlimit).each do |i|
  if $primes[i]
    n = i + i
    while n < $upperlimit
      $primes[n] = false
      n = n + i
    end
  end
end

def phi(n)
  return n - 1 if $primes[n]
  prod = n
  accum = 1
  (2 .. n).each do |p|
    if $primes[p] and n % p == 0
      prod = prod * (1 - 1 / p.to_f)
      while n % (accum * p) == 0
        accum = accum * p
      end
      if accum == n
        break
      end
    end
  end
  prod.to_i
end

sum = 0
(2..1000000).each do |i|
  if i % 10000 == 0
    puts i
  end
  sum = sum + phi(i)
end
puts sum
