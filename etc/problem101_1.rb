
# I'm flattening this down to one main function to make it easier to
# translate into whatever.

sum = 0
(0..9).each do |i|
  (0..i).each do |j|
    prod1, prod2 = 1, 1
    (0..i).each do |k|
      if k != j
        prod1 *= (i + 1 - k).to_f
        prod2 *= (j - k).to_f
      end
    end
    prod = prod1 / prod2
    poly = 1 - (j + 1) + (j + 1) ** 2 - (j + 1) ** 3 + (j + 1) ** 4 - (j + 1) ** 5 + (j + 1) ** 6 - (j + 1) ** 7 + (j + 1) ** 8 - (j + 1) ** 9 + (j + 1) ** 10
    sum += poly * prod
  end
end
puts sum.round

=begin

TOP
j
i
sum'
BOTTOM

at inner inner loop
TOP
k
prod2
prod1
j
i
sum
BOTTOM

at inner loop
TOP
j
i
sum
BOTTOM

at outer loop
TOP
i
sum
BOTTOM

=end
