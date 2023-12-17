// Advent of Code 2023. Day 18: ?
// dotnet fsi aoc18.fsx

open System
open System.IO

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName
    ()

"sample" |> run