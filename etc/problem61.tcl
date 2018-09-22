
proc triangular {n} {
    expr {$n * ($n + 1) / 2}
}

proc square {n} {
    expr {$n * $n}
}

proc pentagonal {n} {
    expr {$n * (3 * $n - 1) / 2}
}

proc hexagonal {n} {
    expr {$n * (2 * $n - 1)}
}

proc heptagonal {n} {
    expr {$n * (5 * $n - 3) / 2}
}

proc octagonal {n} {
    expr {$n * (3 * $n - 2)}
}

proc generateFours {f} {
    set acc [list]
    for {set i 0} true {incr i} {
        set curr [$f $i]
        if {[string length $curr] > 4} {
            break
        } elseif {[string length $curr] == 4} {
            lappend acc $curr
        }
    }
    return $acc
}

proc first {str} {
    string range $str 0 1
}

proc last {str} {
    string range $str 2 3
}

proc tryChain {chain repr} {
    global lookup
    if {[llength $repr] == 6} {
        if {[last [lindex $chain 0]] == [first [lindex $chain 5]]} {
            return $chain
        } else {
            return [list]
        }
    }
    set start [last [lindex $chain 0]]
    dict for {key value} $lookup {
        if {[first $key] != $start} {
            continue
        }
        if {[lsearch -exact $repr $value] >= 0} {
            continue
        }
        set repr1 [linsert $repr 0 $value]
        set chain1 [linsert $chain 0 $key]
        set result [tryChain $chain1 $repr1]
        if {[llength $result] > 0} {
            return $result
        }
    }
    return [list]
}

set elems(3) [generateFours triangular]
set elems(4) [generateFours square]
set elems(5) [generateFours pentagonal]
set elems(6) [generateFours hexagonal]
set elems(7) [generateFours heptagonal]
set elems(8) [generateFours octagonal]

set lookup [dict create]
for {set i 3} {$i <= 8} {incr i} {
    foreach value $elems($i) {
        dict set lookup $value $i
    }
}

# We start with octagonal since that's the smallest list to iterate over
set result {}
foreach value $elems(8) {
    set result [tryChain $value [list 8]]
    if {[llength $result] > 0} {
        break
    }
}
set sum 0
foreach n $result {
    incr sum $n
}
puts $sum
