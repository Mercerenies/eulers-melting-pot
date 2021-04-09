
proc deleteLast {lst} {
    set lastindex [expr {[llength $lst] - 1}]
    lreplace $lst $lastindex $lastindex
}

proc expectedDistribution {turns} {
    set red 1
    set blue 1
    set distribution [list]
    for {set turn 1} {$turn <= $turns} {incr turn} {
        lappend distribution [expr {double($blue) / ($red + $blue)}]
        incr red
    }
    return $distribution
}

proc bruteForce {distribution n} {
    if {$n <= 0} {
        return 1.0
    } elseif {[llength $distribution] == 0} {
        return 0.0
    } else {
        set suffix [deleteLast $distribution]
        set current [lindex $distribution [expr {[llength $distribution] - 1}]]
        expr {[bruteForce $suffix [expr {$n - 1}]] * $current + [bruteForce $suffix $n] * [expr {1 - $current}]}
    }
}

set distr [expectedDistribution 15]
puts [expr {floor(1.0 / [bruteForce $distr 8])}]
