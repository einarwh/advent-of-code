// Advent of Code 2018. Day 1: Chronal Calibration
// dotnet fsi aoc01.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let rec solve seen current index (changes : int array) = 
    if List.contains current seen then current 
    else 
        let seen' = current :: seen 
        let next = current + changes[index] 
        let ix = (index + 1) % changes.Length
        solve seen' next ix changes

let run fileName = 
    let lines = readLines fileName
    let changes = lines |> Array.map int
    changes |> Array.sum |> printfn "%d"
    changes |> solve [] 0 0 |> printfn "%d"

"sample3" |> run 
