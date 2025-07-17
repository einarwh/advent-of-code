echo "Day: $1"
mkdir $1
touch $1/sample
touch $1/input
cat > $1/aoc$1.fsx << EOF
// Advent of Code 2019. Day $1
// dotnet fsi aoc$1.fsx

open System
open System.IO

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    lines |> printfn "%A"
    let text = readText fileName
    text |> printfn "%s"

run "input"
EOF
