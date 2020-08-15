
$total = 0
File.foreach("./files/p089_roman.txt") do |line|
  $total += 1 if line =~ /VIIII/
  $total += 2 if line =~ /IIII/
  $total += 1 if line =~ /LXXXX/
  $total += 2 if line =~ /XXXX/
  $total += 1 if line =~ /DCCCC/
  $total += 2 if line =~ /CCCC/
end
puts $total
