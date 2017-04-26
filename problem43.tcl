
proc swap {var i j} {
    set temp [string replace $var $i $i [string index $var $j]]
    string replace $temp $j $j [string index $var $i]
}

proc nextPerm {varName} {
    upvar $varName var
    set pivot no
    for {set i [expr {[string length $var] - 2}]} {$i >= 0} {incr i -1} {
        if {[string index $var $i] > [string index $var [expr {$i + 1}]]} then {
            set pivot $i
            break
        }
    }
    if {$pivot eq {no}} then {
        return no
    }
    for {set i [expr {[string length $var] - 1}]} {$i > $pivot} {incr i -1} {
        if {[string index $var $pivot] > [string index $var $i]} then {
            set rel $i
            break
        }
    }
    set var [swap $var $rel $pivot]
    set part [string reverse [string range $var [expr {$pivot + 1}] end]]
    set var [string replace $var [expr {$pivot + 1}] end $part]
    return yes
}

proc zz {val} {
    regsub -- {^0*} $val {}
}

set state "9876543210"

set sum 0

while {[nextPerm state]} {
    if {[zz [string range $state 1 3]] %  2 != 0} then { continue }
    if {[zz [string range $state 2 4]] %  3 != 0} then { continue }
    if {[zz [string range $state 3 5]] %  5 != 0} then { continue }
    if {[zz [string range $state 4 6]] %  7 != 0} then { continue }
    if {[zz [string range $state 5 7]] % 11 != 0} then { continue }
    if {[zz [string range $state 6 8]] % 13 != 0} then { continue }
    if {[zz [string range $state 7 9]] % 17 != 0} then { continue }
    incr sum $state
}

puts $sum
