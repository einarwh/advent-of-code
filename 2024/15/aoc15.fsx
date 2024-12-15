// Advent of Code 2024. Day 15: Warehouse Woes.
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

run "input"
