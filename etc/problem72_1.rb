
$upperlimit = 1000000

$phi = (0..$upperlimit).to_a

(2..$upperlimit).each do |i|
  if $phi[i] == i # If we're dealing with a prime number
    n = i
    while n <= $upperlimit
      $phi[n] = $phi[n] * (1 - 1 / i.to_f)
      n = n + i
    end
  end
end
$phi = $phi.map(&:to_i)

puts $phi.sum - 1 # Subtract 1 because we're talking about proper fractions, and phi(1) = 1
