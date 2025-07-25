// Advent of Code 2016. Day 21: Scrambled Letters and Hash.
// dotnet fsi aoc21.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    lines |> printfn "%A"
    let text = File.ReadAllText(fileName).Trim()
    text |> printfn "%s"
    let password = "abcdefgh"
    password |> printfn "%s"

run "input.txt"
