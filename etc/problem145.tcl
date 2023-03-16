
# So brute forcing is a bust.

proc chars {str} {
    split $str ""
}

set total 0
for {set i 0} {$i < 1000000000} {incr i} {
    # Skip any numbers with a trailing zero
    if {$i % 10 != 0} {
        set sum [expr $i + [string reverse $i]]
        set allodd 1
        foreach digit [chars $sum] {
            if {$digit % 2 == 0} {
                set allodd 0
            }
        }
        if {$allodd} {
            incr total
        }
    }
    if {$i % 100000 == 0} {
        puts $i
    }
}
puts $total
