// Advent of Code 2018. Day 08: Memory Maneuver.
// dotnet fsi aoc08.fsx

open System
open System.IO

let parse (s : string) = 
    s.Split(" ") |> Array.map int 

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> parse |> printfn "%A"

run "input.txt"
