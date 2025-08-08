// Advent of Code 2016. Day 17: Two Steps Forward.
// dotnet fsi aoc17.fsx

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
    "Not solved." |> printfn "%s"

run "input.txt"
