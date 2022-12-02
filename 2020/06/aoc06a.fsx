// Advent of Code 2020. Day 6, Part A.
// dotnet fsi aoc06a.fsx

open System.IO

let run (text : string) =
    text.Split("\n\n")
    |> Array.map (fun s -> s.Replace("\n", "") |> Seq.distinct |> Seq.length)
    |> Array.sum
    |> printfn "%d"

"input" |> File.ReadAllText |> run 
