// Advent of Code 2017. Day 23: Coprocessor Conflagration.
// dotnet fsi aoc23.fsx

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
    "Not solved." |> printfn "%s"

run "input"
