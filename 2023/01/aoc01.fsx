// Advent of Code 2023. Day 1: Calorie Counting.
// dotnet fsi aoc01.fsx

open System
open System.IO

let tryParseInt (ch : char) : int option = 
    if Char.IsDigit ch then 
        Some (Char.GetNumericValue ch |> int)
    else 
        None

let toNum (nums : int seq) : int = 
    let tens = nums |> Seq.head
    let ones = nums |> Seq.rev |> Seq.head 
    tens * 10 + ones

let parseLine (line : string) : int =
    line |> Seq.choose tryParseInt |> toNum

let part1 filename = 
    filename 
    |> File.ReadAllLines
    |> Array.toList
    |> List.filter (fun line -> line <> String.Empty)
    |> List.map parseLine
    |> List.sum

let run filename = 
    part1 filename |> printfn "%d"

run "input"
