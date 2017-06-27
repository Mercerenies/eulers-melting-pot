#!/usr/bin/ruby

@sieve = 9999.times.collect { true }.to_a
(2..9999).each do |n|
  next unless @sieve[n]
  i = n
  while i <= 9999
    i += n
    @sieve[i] = false
  end
end
puts "Sieve done..."

def prime?(x)
  @sieve[x]
end

def permute?(x, y)
  x.to_s.split(//).permutation.collect(&:join).detect(&y.to_s.method(:==))
end

(1000..9999).each do |n|
  next if n == 1487 # Ignore the known solution
  puts n
  (n+1..9999).each do |m|
    k = m * 2 - n
    if prime? n and prime? m and prime? k and permute? n, m and permute? n, k
      puts "#{n}#{m}#{k}"
      exit 0
    end
  end
end
