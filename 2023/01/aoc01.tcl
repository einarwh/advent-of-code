# Advent of Code 2023. Day 1: Trebuchet?!
# tclsh aoc01.tcl

set fp [open "input" r]
set file_data [read $fp]
close $fp

proc toNumber {numbers} {
    set result 0
    if {[llength $numbers] > 0} {
        set tens [lindex $numbers 0]
        set ones [lindex $numbers [expr [llength $numbers] - 1]]
        set result [expr [expr $tens * 10] + $ones]
    }
    return $result
}

proc getScore1 {line} {
    set numbers [list]
    foreach char [split $line ""] {
        if {[string is integer -strict $char]} {
            lappend numbers $char
        }
    }
    return [toNumber $numbers]
}

proc getScore2 {line} {
    set len [string length $line]
    set numbers [list]
    for {set i 0} {$i < $len} {incr i} {
        set char [string index $line $i]
        set substr [string range $line $i $len]
        if {[string is integer -strict $char]} {
            lappend numbers $char
        } elseif {[string equal -length 3 "one" $substr]} {
            lappend numbers 1
        } elseif {[string equal -length 3 "two" $substr]} {
            lappend numbers 2
        } elseif {[string equal -length 5 "three" $substr]} {
            lappend numbers 3
        } elseif {[string equal -length 4 "four" $substr]} {
            lappend numbers 4
        } elseif {[string equal -length 4 "five" $substr]} {
            lappend numbers 5
        } elseif {[string equal -length 3 "six" $substr]} {
            lappend numbers 6
        } elseif {[string equal -length 5 "seven" $substr]} {
            lappend numbers 7
        } elseif {[string equal -length 5 "eight" $substr]} {
            lappend numbers 8
        } elseif {[string equal -length 4 "nine" $substr]} {
            lappend numbers 9
        }
    }
    return [toNumber $numbers]
}

set data [split $file_data "\n"]
set total1 0
set total2 0
foreach line $data {
    set score1 [getScore1 $line]
    set total1 [expr $total1 + $score1]
    set score2 [getScore2 $line]
    set total2 [expr $total2 + $score2]
}
puts $total1
puts $total2