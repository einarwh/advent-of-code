// Advent of Code 2015. Day 24: It Hangs in the Balance.
// dotnet fsi aoc24.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let packages = lines |> List.map int 
    packages |> List.sum |> printfn "%A"
    "Not solved." |> printfn "%s"

run "sample.txt"
