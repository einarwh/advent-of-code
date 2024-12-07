// Advent of Code 2024. Day 07
// dotnet fsi aoc07.fsx

open System
open System.IO

let split (splitter : string) (input : string) = input.Split(splitter)

let parseLine (s : string) = 
    let parts = s |> split ": "
    let testValue = int64 parts.[0]
    let numbers = parts.[1] |> split " " |> Array.toList |> List.map int64 
    (testValue, numbers)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let checkEquation (testValue : int64, numbers : int64 list) : int64 option  = 
    let rec check result nums = 
        match nums with 
        | [] -> result = testValue
        | n :: rest -> 
            let sum = result + n 
            let prod = result * n 
            (sum <= testValue && check sum rest) || (prod <= testValue && check prod rest)
    match numbers with 
    | [] -> None 
    | h :: t -> 
        if check h t then Some testValue else None

let run fileName = 
    let lines = readLines fileName
    let equations = lines |> List.map parseLine 
    equations |> List.choose checkEquation |> List.sum |> printfn "%d"

run "input"
