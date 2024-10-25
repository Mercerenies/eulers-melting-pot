
proc findNextChar {remainingCount} {
    puts "# Find a valid character to use next ($remainingCount to go)"
    puts "1t1"
    puts "3t2"
    puts "2s/$/ /"
    puts "2,3j"
    for {set i 0} {$i < $remainingCount - 1} {incr i} {
        puts {s/\(.\)\(.*\) \(.*\)\1,/\2 \3\1,/}
    }
    puts {s/ .*$//}
    puts {m$}

    puts "# Remove the character from the valid characters list"
    puts [string cat {1s/\(.\)/\1} "\\\n" {/g}]
    for {set i [expr $remainingCount]} {$i > 0} {set i [expr {$i - 1}]} {
        puts [string cat "\$t" $i]
        puts [string cat $i "," [expr {$i + 1}] "j"]
    }
    puts [string cat "1,$remainingCount" "g" {/\(.\)\1/} "d"]
    puts [string cat "1," [expr {$remainingCount - 1}] {s/.$//}]
    puts [string cat "1," [expr {$remainingCount - 1}] "j"]
    puts "2d"

    puts "# Remove the character from any constraints"
    puts "2s/,/\\\n/g"
    puts {$y}
    puts {2,/^$/-1g/.*/x}
    puts {2,/^$/-1g/../j}
    puts {2,/^$/-1g/\(.\).\1/d}
    if {$remainingCount > 2} {
        puts {2,/^$/-1g/.../s/.$/,/}
    }
    puts {2,/^$/j}
}

puts "# Generate sequences of 2-digit constraints"
puts [string cat "1,$" {s/\(.\)\(.\)\(.\)/\1\2} "\\\n" {\2\3/}]

#puts "# Sort the list"
#for {set i 9} {$i >= 0} {set i [expr $i - 1]} {
#    for {set j 9} {$j >= 0} {set j [expr $j - 1]} {
#      puts "g/$i$j/m0"
#    }
#}

puts "# Concatenate all lines"
puts {1,$s/$/,/}
puts {1,$j}

puts "# Make a list of valid characters"
puts "0a\n01236789\n."

for {set i 8} {$i > 1} {set i [expr $i - 1]} {
    findNextChar $i
}

puts "# Clean up the output"
puts "2d"
puts "1m$"
puts {1,$jp}
puts "Q"
