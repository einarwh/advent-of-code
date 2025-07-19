// Advent of Code 2016. Day 09: Explosives in Cyberspace.
// dotnet fsi aoc09.fsx

open System
open System.IO

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    text |> printfn "%s"

run "input.txt"
