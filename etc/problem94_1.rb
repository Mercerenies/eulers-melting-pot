
# A couple hours of reading the theory of Diophantine equations later,
# and I think I have something.

def next_dio_sol(xy)
  x, y = xy
  return 2 * x + 3 * y, x + 2 * y
end

sol = [2, 0]
total_perimeter = 0
loop do
  x, y = sol

  # Case 1: a = b = c - 1
  if (x + 1) % 6 == 3 # Mod 3 should be zero and Div 3 should be odd
    #a = (x + 1) / 3
    p = x + 2
    total_perimeter += p
  end

  # Case 1: a = b = c + 1
  if (x - 1) % 6 == 3 # Mod 3 should be zero and Div 3 should be odd
    #a = (x - 1) / 3
    p = x - 2
    total_perimeter += p
  end

  sol = next_dio_sol(sol)
  puts "#{sol}"
  break if sol[0] > 999999998
  puts total_perimeter
end

# We're close, but we're including two undesired cases: (1, 1, 0) and
# (1, 1, 2). This is a total perimeter of 6, so subtract 6.
puts total_perimeter - 6
