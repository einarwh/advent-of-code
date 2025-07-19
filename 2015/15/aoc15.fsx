// Advent of Code 2015. Day 15
// dotnet fsi aoc15.fsx

open System
open System.IO

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
