
def factorial(n)
  1.upto(n).reduce(1, :*)
end

def sum_of_digit_fact(n)
  n.to_s.each_char.map { |x| factorial(x.to_i) }.sum
end

$cache = {}
$cache[169] = 3
$cache[363601] = 3
$cache[1454] = 3
$cache[871] = 2
$cache[45361] = 2
$cache[872] = 2
$cache[45362] = 2

def non_repeating_length(n)
  if not $cache.include? n
    n1 = sum_of_digit_fact n
    if n1 == n
      $cache[n] = 1
    else
      $cache[n] = 1 + non_repeating_length(n1)
    end
  end
  $cache[n]
end

count = 0
1.upto(1000000).each do |x|
  if non_repeating_length(x) == 60
    count += 1
  end
end
puts count
