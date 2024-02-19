
# Naive solution. See 180_analysis.hs for details on the approach.
# We're looking for (x, y, z) rational such that either x + y = ±z or
# x^2 + y^2 = z^2.
#
# About 1 second in Ruby. Correct.

require 'set'

$all_solutions = Set[]
$k = 35

# We only care about values up to 35, so memoize all the square roots
# we could ever need.
$sqrts = (0..$k).map { |x| [x * x, x] }.to_h

def isqrt(n)
  $sqrts[n]
end

def sqrt(r)
  numer = isqrt(r.numerator)
  denom = isqrt(r.denominator)
  numer and denom and Rational(numer, denom)
end

def bounds?(z)
  z > 0 and
    z.numerator.between?(1, $k) and
    z.denominator.between?(1, $k) and
    z.numerator < z.denominator
end

def pretty_rational(r)
  # Pretty-print rational number in a Haskell-compatible format.
  "#{r.numerator} % #{r.denominator}"
end

def add_solution(x, y, z)
  return unless z and bounds?(z)
  #puts "[#{pretty_rational(x)}, #{pretty_rational(y)}, #{pretty_rational(z)}]"
  $all_solutions.add(x + y + z)
end

def produce_solutions_for(x, y)
  solutions = []
  # x + y = z
  z = x + y
  add_solution x, y, z
  # x^2 + y^2 = z^2
  z = sqrt(x ** 2 + y ** 2)
  add_solution x, y, z
  # x^(-1) + y^(-1) = z^(-1)
  z = x * y / (x + y)
  add_solution x, y, z
  # x^(-2) + y^(-2) = z^(-2)
  z = sqrt((x * y) ** 2 / (x ** 2 + y ** 2))
  add_solution x, y, z
end

def each_fraction(&block)
  if block
    (1..35).each do |a|
      ((a+1)..35).each do |b|
         block.call Rational(a, b)
       end
    end
  else
    Enumerator.new { |y| each_fraction(&y) }
  end
end

each_fraction do |x|
  each_fraction do |y|
    produce_solutions_for(x, y)
  end
end

final_total = $all_solutions.sum
puts(final_total.numerator + final_total.denominator)
