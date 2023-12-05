// Advent of Code 2023. 
// dotnet fsi aoc10.fsx

open System
open System.IO
open System.Text.RegularExpressions

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    printfn "%A" lines 

"sample" |> run 
