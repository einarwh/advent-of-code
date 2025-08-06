// Advent of Code 2020. Day 3, Part A.
// dotnet fsi aoc03a.fsx

open System.IO

let lines = "input.txt" |> File.ReadAllLines
let len = (Array.head lines).Length

lines
|> Array.mapi (fun i line -> if '#' = line.[(3 * i) % len] then 1 else 0)
|> Array.sum
|> printfn "%d"
