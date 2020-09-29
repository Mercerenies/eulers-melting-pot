
def poly(n)
  1 - n + n ** 2 - n ** 3 + n ** 4 - n ** 5 + n ** 6 - n ** 7 + n ** 8 - n ** 9 + n ** 10
end

def lbasis(deg, x, j)
  prod = 1
  (0..deg).each do |k|
    next if k == j
    prod *= (x - (k + 1)).to_f / (j - k)
  end
  prod
end

def interpolating(deg, x)
  sum = 0
  (0..deg).each do |j|
    sum += poly(j + 1) * lbasis(deg, x, j)
  end
  sum
end

sum = 0
(0..9).each do |i|
  sum += interpolating(i, i + 2)
end
puts sum.round
