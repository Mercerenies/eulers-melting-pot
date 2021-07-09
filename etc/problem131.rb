
require 'prime'

def cube_impl(k)
  test = 1
  test += 1 while test * test * test < k
  test * test * test == k
end

$cubes = {}

def cube?(k)
  $cubes[k] = cube_impl(k) unless $cubes.include? k
  $cubes[k]
end

def find_n(p)
  (1..(p**2)).find do |n|
    test_value = n ** 3 + p * n ** 2
    cube? test_value
  end
end

(1..100).each do |p|
  n = find_n(p)
  if Prime.prime?(p) && n
    puts "#{p} #{n} #{n ** 3 + p * n ** 2}"
  end
end
