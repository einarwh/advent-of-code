// Advent of Code 2023. Day 19: ?
// dotnet fsi aoc19.fsx

open System
open System.IO

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName
    ()

"sample" |> run