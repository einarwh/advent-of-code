// Advent of Code 2015. Day 16: Aunt Sue.
// dotnet fsi aoc16.fsx

open System
open System.IO

let message = [
    ("children", 3)
    ("cats", 7)
    ("samoyeds", 2)
    ("pomeranians", 3)
    ("akitas", 0)
    ("vizslas", 0)
    ("goldfish", 5)
    ("trees", 3)
    ("cars", 2)
    ("perfumes", 1)
]

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    lines |> printfn "%A"
    let text = File.ReadAllText(fileName).Trim()
    text |> printfn "%s"

run "input.txt"
