require 'set'

def condition1(nums)
  sums = Set[0]
  nums.each do |x|
    sums1 = []
    sums.each do |y|
      sums1 << (x + y)
    end
    sums1.each do |z|
      return false if sums.include? z
      sums.add(z)
    end
  end
  true
end

def condition2(nums)
  nums = nums.sort
  a = nums[0]
  b = 0
  (0..nums.length/2).each do |i|
    a += nums[i + 1]
    b += nums[- i - 1]
    return false if a <= b
  end
  true
end

def valid(nums)
  condition1(nums) and condition2(nums)
end

total = 0
File.open('./files/p105_sets.txt') do |f|
  f.each_line do |line|
    nums = line.split(',').map(&:to_i)
    total += nums.sum if valid nums
  end
end
puts "#{total}"
