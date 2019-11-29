
def dio s
  a0 = (s ** 0.5).floor
  return 0 if a0 ** 2 == s

  m = 0
  d = 1
  a = a0

  n1 = 1
  n0 = a

  d1 = 0
  d0 = 1

  loop do

    if n0 * n0 - s * d0 * d0 == 1
      puts n0 if s == 802
      return n0
    end

    m = d * a - m
    d = (s - m * m) / d
    a = ((a0 + m) / d).floor

    n0, n1 = a * n0 + n1, n0
    d0, d1 = a * d0 + d1, d0

  end
end

maxd = nil
maxx = 0
(1..1000).each do |d|
  x = dio d
  if x > maxx
    maxx = x
    maxd = d
  end
end
puts maxd
puts maxx
