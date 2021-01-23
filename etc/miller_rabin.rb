
# https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test

Tests = 4

LowPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43]

def is_prime(n)
  # Dumb special cases that I don't care about.
  return false if n < 2
  return true if LowPrimes.include? n
  return false if LowPrimes.any? { |x| n % x == 0 }

  d = n - 1
  r = 0
  while d % 2 == 0
    d /= 2
    r += 1
  end
  Tests.times do
    catch :next do
      a = 2 + rand(n - 3)
      x = (a ** d) % n
      throw :next if x == 1 or x == n - 1
      (r - 1).times do
        x = (x ** 2) % n
        throw :next if x == n - 1
      end
      return false
    end
  end
  true
end

def is_prime_trial(n)
  return false if n < 2
  (2..(n-1)).each do |i|
    return false if n % i == 0
  end
  true
end

count = 0
(1..100000).each do |i|
  puts i if i % 1000 == 0
  count += 1 if is_prime(i)
end
puts count
