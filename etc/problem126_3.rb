
def g(x, y, z, k)
  2 * (x * z + y * z + 2 * k * z + x * y + 2 * k * (x + y + k - 1))
end

def c(n)
  if n % 2 == 0
    count = 0
    (0..).each do |k|
      break if g(1, 1, 1, k) > n
      (1..).each do |x|
        break if g(x, 1, 1, k) > n
        (1..x).each do |y|
          break if g(x, y, 1, k) > n
          (1..y).each do |z|
            curr = g x, y, z, k
            if curr == n
              count += 1
            end
            break if curr >= n
          end
        end
      end
    end
    count
  else
    0
  end
end

def c1(n)
  if n % 2 == 0
    count = 0
    (0..).each do |k|
      break if g(1, 1, 1, k) > n
      (1..).each do |x|
        break if g(x, 1, 1, k) > n
        (1..x).each do |y|
          break if g(x, y, 1, k) > n
          numer = n/2 - (x * y + 2 * k * (x + y + k - 1))
          denom = x + y + 2 * k
          if numer % denom == 0 && numer / denom > 0 && numer / denom <= y
            count += 1
          end
        end
      end
    end
    count
  else
    0
  end
end

m = 0
(1..).each do |n|
  if c1(n) == 1000
    puts "#{n}"
    break
  end
  m = [m, c1(n)].max
  if n % 1000 == 0
    puts "#{n} #{m}"
  end
end
