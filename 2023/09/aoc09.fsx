// Advent of Code 2023. Day 9: Mirage Maintenance
// dotnet fsi aoc09.fsx

open System
open System.IO

let parseLine (s : string) = 
    s.Split(" ") |> Array.toList |> List.map int64

let differences numbers = 
    numbers |> List.pairwise |> List.map (fun (a, b) -> (b - a)) 

let historyValue numbers = 
    let rec loop numbers acc = 
        if List.forall (fun n -> n = 0L) numbers then 
            List.sum acc
        else 
            let diffs = differences numbers 
            let last = List.last numbers
            loop diffs (last :: acc)
    loop numbers []

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    lines 
    |> List.sumBy (parseLine >> historyValue)
    |> printfn "%d"

"input" |> run 
