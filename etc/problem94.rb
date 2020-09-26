# coding: utf-8

# Let's try brute forcing it. :)

# We're using Heron's formula here. I've already expanded it out on
# paper to get a simpler form, given that we know that a = b = c ± 1.

def is_square(n)
  Integer.sqrt(n) ** 2 == n
  #1.step do |i|
  #  return false if i * i > n
  #  return true if i * i == n
  #end
end

total_perimeter = 0
(3..333333333).each do |a|
  next if a % 2 == 0
  # a = b = c - 1 case
  area2 = (3 * a + 1) * (a - 1)
  #puts "CASE 1: #{a}" if is_square(area2)
  total_perimeter += 3 * a + 1 if is_square(area2)
  # a = b = c + 1 case
  area2 = (3 * a - 1) * (a + 1)
  #puts "CASE 2: #{a}" if is_square(area2)
  total_perimeter += 3 * a - 1 if is_square(area2)
end
puts total_perimeter
