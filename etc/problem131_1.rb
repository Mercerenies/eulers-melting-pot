
# Project Euler has really got to stop throwing challenges out there
# that are, verbatim, on OEIS. Brute-forced the first four
# (problem130.rb), and then OEIS took it from there.
#
# https://oeis.org/A338610

require 'prime'

count = 0
k = 0
loop do
  p = 3 * k * k + 3 * k + 1
  break if p >= 1000000
  count += 1 if Prime.prime? p
  k += 1
end
puts count
