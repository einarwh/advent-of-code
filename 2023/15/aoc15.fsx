// Advent of Code 2023. Day 15: Lens Library
// dotnet fsi aoc15.fsx

open System
open System.IO

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let hash (s : string) : int = 
    let folder current ch  = 
        ((current + int ch) * 17) % 256
    s |> Seq.toList |> List.fold folder 0

let run fileName =
    let lines = readLines fileName |> Array.toList
    "cm-" |> hash |> printfn "%d"

"sample" |> run
