
# Based on this algorithm: https://www.mathblog.dk/project-euler-66-diophantine-equation/

result = 0
largest = 0

(2..1000).each do |curr|

  limit = (curr ** 0.5).floor
  # Skip perfect squares
  next if limit * limit == curr

  m = 0
  d = 1
  a = limit

  num1 = 1
  num = a

  den1 = 0
  den = 1

  while num * num - curr * den * den != 1
    m = d * a - m
    d = (curr - m * m) / d
    a = (limit + m) / d

    num, num1 = a * num + num1, num
    den, den1 = a * den + den1, den
  end

  if num > largest
    largest = num
    result = curr
  end

end

puts result
