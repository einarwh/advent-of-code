// Advent of Code 2020. 
// Day 13: Shuttle Search, Part A.
// dotnet fsi aoc13a.fsx

open System.IO

let run (lines : string array) =
    let timestamp = int lines.[0]
    lines.[1].Split(',')
    |> Array.toList
    |> List.filter (fun s -> s <> "x")
    |> List.map int
    |> List.map (fun b -> (b, b - timestamp % b))
    |> List.minBy snd
    |> (fun (bus, minutes) -> bus*minutes)
    |> printfn "%d"

"input.txt" |> File.ReadAllLines |> run