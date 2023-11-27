# Advent of Code 2022. Day 2: Rock Paper Scissors.
# tclsh aoc02.tcl

set fp [open "input" r]
set file_data [read $fp]
close $fp

proc getScore1 {line} {
    if {$line == "A X"} {
        return 4
    } elseif {$line == "A Y"} {
        return 8
    } elseif {$line == "A Z"} {
        return 3
    } elseif {$line == "B X"} {
        return 1
    } elseif {$line == "B Y"} {
        return 5
    } elseif {$line == "B Z"} {
        return 9
    } elseif {$line == "C X"} {
        return 7
    } elseif {$line == "C Y"} {
        return 2
    } elseif {$line == "C Z"} {
        return 6
    } else {
        return 0
    }
}

proc getScore2 {line} {
    if {$line == "A X"} {
        return 3
    } elseif {$line == "A Y"} {
        return 4
    } elseif {$line == "A Z"} {
        return 8
    } elseif {$line == "B X"} {
        return 1
    } elseif {$line == "B Y"} {
        return 5
    } elseif {$line == "B Z"} {
        return 9
    } elseif {$line == "C X"} {
        return 2
    } elseif {$line == "C Y"} {
        return 6
    } elseif {$line == "C Z"} {
        return 7
    } else {
        return 0
    }
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
