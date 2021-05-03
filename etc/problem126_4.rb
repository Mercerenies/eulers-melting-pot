
def g(x, y, z, k)
  2 * (x * z + y * z + 2 * k * z + x * y + 2 * k * (x + y + k - 1))
end

LIMIT = 40000

c = {}
c.default = 0

(1..).each do |z|
  break if g(z, z, z, 0) > LIMIT
  (z..).each do |y|
    break if g(y, y, z, 0) > LIMIT
    (y..).each do |x|
      break if g(x, y, z, 0) > LIMIT
      (0..).each do |k|
        break if g(x, y, z, k) > LIMIT
        c[g(x, y, z, k)] += 1
      end
    end
  end
end

(1..).each do |k|
  if c[k] == 1000
    puts "#{k}"
    break
  end
end
