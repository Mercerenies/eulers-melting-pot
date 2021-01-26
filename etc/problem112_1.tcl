
# Let's get rid of those pesky string operations, eh?

proc isBouncy {n} {
    set down false
    set up false
    set a {}
    set b [expr {$n % 10}]
    set n [expr {$n / 10}]
    while {$n != 0} {
        set a $b
        set b [expr {$n % 10}]
        set n [expr {$n / 10}]
        if {$a > $b} {
            set down true
        }
        if {$a < $b} {
            set up true
        }
    }
    return [expr {$up && $down}]
}

set count 0
set i 100
while {$count != 0.99 * $i} {
    incr i
    if {[isBouncy $i]} {
        incr count
    }
}
puts $i

# Discord: count
# Applejack: i
# Rainbow Dash: 100 * Discord
# Rainbow Crash: 99 * Applejack
# Pinkie Pie: n
# Spike: isBouncy
# Sweetie Bell: down
# Twilight: up
# Butternut: a
# Oak Nut: b
