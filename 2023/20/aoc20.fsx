// Advent of Code 2023. Day 20: ?
// dotnet fsi aoc20.fsx

open System
open System.IO

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName
    ()

"sample" |> run