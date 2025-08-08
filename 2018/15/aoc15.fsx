// Advent of Code 2018. Day 15: Beverage Bandits.
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
    "Not solved." |> printfn "%s"

run "input.txt"
