
# Let's dumb it down

proc distrGet {n} {
    expr {1.0 / (1.0 + $n)}
}

proc bruteForce {x n} {
    if {$n <= 0} {
        return 1.0
    } elseif {$x == 0} {
        return 0.0
    } else {
        set x1 [expr {$x - 1}]
        set current [distrGet $x]
        expr {[bruteForce $x1 [expr {$n - 1}]] * $current + [bruteForce $x1 $n] * [expr {1 - $current}]}
    }
}

puts [expr {floor(1.0 / [bruteForce 15 8])}]

# TOP OF STACK
# b(x-1,n)
# (1-current)
# current*b(x-1,n-1)

# VARS
# b = bruteForce
# A = b(x-1, n)
# B = b(x-1, n-1)
