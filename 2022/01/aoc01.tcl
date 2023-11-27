set fp [open "input" r]
set file_data [read $fp]
close $fp

set cal 0
set data [split $file_data "\n"]
set elves [list]
foreach line $data {
    if {[string length $line] == 0} {
        lappend elves $cal
        set cal 0
    } else {
        set cal [expr $cal + $line]
    }
}

set sorted [lsort -integer -decreasing $elves]
set first [lindex $sorted 0]
set second [lindex $sorted 1]
set third [lindex $sorted 2]
puts "Most calories: $first"
set topthree [expr $first + $second + $third]
puts "Top three: $topthree"
