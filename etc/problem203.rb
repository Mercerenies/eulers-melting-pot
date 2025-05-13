
def squarefree?(n)
  i = 2
  while i * i <= n
    return false if (n % (i * i)).zero?
    i += 1
  end
  true
end

def pascals_triangle
  Enumerator.produce([1]) do |arr|
    [1] + arr.each_cons(2).map(&:sum) + [1]
  end
end

puts pascals_triangle.take(51).flatten.uniq.select { |n| squarefree?(n) }.sum
