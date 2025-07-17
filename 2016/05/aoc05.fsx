// Advent of Code 2016. Day 05: How About a Nice Game of Chess?
// dotnet fsi aoc05.fsx

open System
open System.IO

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let text = readText fileName
    text |> printfn "%s"

run "input"
