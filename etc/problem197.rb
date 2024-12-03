
# Runs instantaneously, converges very quickly.
#
# Note: This solution uses Ruby rationals, but I already tested it,
# and floats work too.

def f(x)
  exp = 2 ** (30.403243784 - x ** 2)
  exp.floor * 10 ** -9
end

def u(n)
  k = -1
  n.times do
    k = f(k)
  end
  k
end

=begin
k = -1
100.times do
  puts k.to_f
  k = f(k)
end
=end

=begin
# Function seems to converge by this point.
n = 1e5
printf "%.10f\n", u(n.floor) + u(n.floor + 1)
=end

# Ensure 10 digits of precision, so we don't "lose" a digit when we
# add the two numbers together. (cf XKCD 2295)
PRINTF = "%.10f\n"

i = 0
k1 = -1
k2 = f(k1)
loop do
  new_k1 = f(k2)
  new_k2 = f(new_k1)

  if sprintf(PRINTF, new_k1) == sprintf(PRINTF, k1) && sprintf(PRINTF, new_k2) == sprintf(PRINTF, k2)
    printf "%.9f\n", new_k1 + new_k2
    break
  end
  k1 = new_k1
  k2 = new_k2
  i += 1
end
