
# Based on the algorithm on Wikipedia
# https://en.wikipedia.org/wiki/Methods_of_computing_square_roots
def frac_period(s)
  triplets = []
  a0 = (s ** 0.5).floor

  return 0 if a0 ** 2 == s # Perfect squares

  m = 0
  d = 1
  a = a0
  loop do

    index = triplets.find_index [m, d, a]
    if index
      return triplets.length - index
    end
    triplets << [m, d, a]

    m = d * a - m
    d = (s - m ** 2) / d
    a = ((a0 + m) / d).floor

  end
end


print(10001.times.count { |i| frac_period(i).odd? })
