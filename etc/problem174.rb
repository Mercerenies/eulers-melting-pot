
def tiles_needed(n, m)
  n * n - m * m
end

ways = Hash::new
(0..1000001).each do |n|
  ways[n] = 0
end

(3..250001).each do |n|
  m = n - 2
  while m > 0 && tiles_needed(n, m) <= 1000000
    tiles = tiles_needed(n, m)
    ways[tiles] += 1
    m -= 2
  end
end

puts ways.select { |k, v| v > 0 && v <= 10 }.length
