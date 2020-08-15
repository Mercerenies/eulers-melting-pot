
# This works, and quite efficiently, but we can do better >:D

$tr = {
  'M'=>1000,
  'D'=>500,
  'C'=>100,
  'L'=>50,
  'X'=>10,
  'V'=>5,
  'I'=>1,
}
$names = [[1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'],
          [100, 'C'], [90, 'XC'], [50, 'L'], [40, 'XL'],
          [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I']]

def from_numeral(text)
  values = text.each_char.map { |x| $tr[x] }
  values << 0
  values1 = []
  i = 0
  while i < values.length - 1
    x, y = values[i], values[i + 1]
    if x < y
      values1 << y - x
      i += 2
    else
      values1 << x
      i += 1
    end
  end
  values1.sum
end

def to_numeral(number)
  result = ""
  $names.each do |n, name|
    while number >= n
      number -= n
      result += name
    end
  end
  result
end

$total = 0
File.foreach("./files/p089_roman.txt") do |line|
  line = line.chomp
  optimized = to_numeral from_numeral line
  $total += line.length - optimized.length
  puts "#{line} #{optimized} #{line.length - optimized.length}"
end
puts $total
