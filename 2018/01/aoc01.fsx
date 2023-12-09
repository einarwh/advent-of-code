// Advent of Code 2018. Day 1: Chronal Calibration
// dotnet fsi aoc01.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let changes = lines |> List.map int
    changes |> List.sum |> printfn "%d"

"input" |> run 
