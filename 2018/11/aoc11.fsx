// Advent of Code 2018. Day 11: Chronal Charge.
// dotnet fsi aoc11.fsx

open System
open System.IO

let getHundredsDigit n = 
    (n / 100) % 10

let getPowerLevel serialNumber (x, y) = 
    let rackId = x + 10
    let d = getHundredsDigit <| (rackId * y + serialNumber) * rackId 
    d - 5

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> printfn "%s"
    getPowerLevel 57 (122, 79) |> printfn "-5 = %d"
    getPowerLevel 39 (217,196) |> printfn "0 = %d"
    getPowerLevel 71 (101,153) |> printfn "4 = %d"

run "input.txt"
