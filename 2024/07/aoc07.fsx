// Advent of Code 2024. Day 07
// dotnet fsi aoc07.fsx

open System
open System.IO

let split (splitter : string) (input : string) = input.Split(splitter)

let parseLine s = 
    let parts = s |> split ": "
    let testValue = int64 parts.[0]
    let numbers = parts.[1] |> split " " |> Array.toList |> List.map int64 
    (testValue, numbers)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let checkEquation operators (testValue, numbers) = 
    let rec check result nums = 
        match nums with 
        | [] -> result = testValue
        | n :: rest -> 
            let withOp op = 
                let value = op result n 
                value <= testValue && check value rest 
            operators |> List.exists (withOp) 
    match numbers with 
    | [] -> None 
    | h :: t -> 
        if check h t then Some testValue else None

let run fileName = 
    let lines = readLines fileName
    let equations = lines |> List.map parseLine 
    let add = fun (a : int64) (b : int64) -> a + b
    let mul = fun (a : int64) (b : int64) -> a * b 
    let concat = fun (a : int64) (b : int64) -> int64 ((string a) + (string b))
    equations 
    |> List.choose (checkEquation [add; mul])
    |> List.sum 
    |> printfn "%d"
    equations
    |> List.choose (checkEquation [add; mul;concat])
    |> List.sum 
    |> printfn "%d"

run "input"
