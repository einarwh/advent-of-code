// Advent of Code 2022. Day 1: Calorie Counting.
// Create URL-encoded input string for hypercat.
// dotnet fsi hypercat.fsx

open System.IO

let run input = 
    let text = File.ReadAllText input 
    let url = text.Trim().Replace("\n", "%A0")
    printfn "%s" url

run "input"
