#!/usr/bin/ruby

@sieve = 999999.times.collect { true }.to_a
(2..999999).each do |n|
  next unless @sieve[n]
  i = n
  while i <= 999999
    i += n
    @sieve[i] = false
  end
end
puts "Sieve done..."

@seq = @sieve.each_with_index.select { |x, i| x }.collect { |x, i| i }.drop(2)

def prime?(x)
  @sieve[x]
end

n = @seq.size
starter = @seq.reduce &:+
while n > 0
  acc = starter
  if acc <= 999999
    i = 0
    while i + n < @seq.size
      if prime? acc
        puts acc
        puts "#{@seq[i..(i+n)]}"
        exit
      end
      acc -= @seq[i]
      acc += @seq[i + n]
      i += 1
    end
  end
  starter -= @seq[n - 1]
  n -= 1
end
