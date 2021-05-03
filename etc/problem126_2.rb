
def g(x, y, z, n)
  2 * (x * z + y * z + 2 * n * z + x * y + 2 * n * (x + y + n - 1))
end

UP_TO = 2000

c = {}
c.default = 0
(1..UP_TO).each do |x|
  (1..x).each do |y|
    (1..y).each do |z|
      n = 0
      while g(x, y, z, n) < UP_TO
        c[g(x, y, z, n)] += 1
        n += 1
      end
    end
  end

  # We know the x'th value is good now
  if c[x] == 100
    puts x
    break
  end
  puts "#{x} #{c[x]}"
end
