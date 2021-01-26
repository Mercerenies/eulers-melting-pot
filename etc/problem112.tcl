
proc isBouncy {str} {
    set up false
    set down false
    set chars [split $str ""]
    for {set i 0} {$i < [llength $chars] - 1} {incr i} {
        set j [expr {$i + 1}]
        if {[lindex $chars $i] > [lindex $chars $j]} {
            set down true
        }
        if {[lindex $chars $i] < [lindex $chars $j]} {
            set up true
        }
    }
    expr {$up && $down}
}

set count 0
set i 2
while {$count != 0.99 * $i} {
    incr i
    if {[isBouncy $i]} {
        incr count
    }
}
puts $i
