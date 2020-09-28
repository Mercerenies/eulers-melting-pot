
# We're doing more fun Pell's equation stuff here!
#
# Fundamental solution for (a^2 - 2 b^2 = -1): (1, 1)
#
# Fundamental solution for Pell's (a^2 - 2 b^2 = 1): (3, 2)
#
# Recurrence:
# a' = 3a + 4b
# b' = 2a + 3b
#
# Easy to see all solutions will be odd, and we will satisfy a' >= b'
#
# Then we get
#
# Blue = (b + 1) / 2
# Total = (a + 1) / 2

a = 1
b = 1

MinA = 2e12

while a < MinA
  a, b = [3 * a + 4 * b, 2 * a + 3 * b]
end

puts (b + 1) / 2
