
def is_sss(arr)
  # Condition 1
  sums = {0 => true}
  arr.each do |u|
    sums1 = []
    sums.each do |s, _|
      return false if sums.include? s + u
      sums1 << s + u
    end
    sums1.each do |s|
      sums[s] = true
    end
  end
  # Condition 2
  lhs = arr[0]
  rhs = 0
  (1..(arr.length / 2)).each do |i|
    lhs += arr[i]
    rhs += arr[-i]
    return false if lhs < rhs
  end
  # Clear
  true
end

lines = File.open("./files/p105_sets.txt") do |file|
  file.each_line.map do |line|
    line.split(',').map(&:to_i).sort
  end
end

total = 0
lines.each do |line|
  total += line.sum if is_sss(line)
end
puts total
