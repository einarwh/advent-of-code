// Advent of Code 2016. Day 03
// dotnet fsi aoc03.fsx

open System
open System.IO

let parse (s : string) : int*int*int = 
    let numbers = 
        s.Split(" ", StringSplitOptions.RemoveEmptyEntries) 
        |> Array.toList
        |> List.map int
    match numbers with 
    | [a; b; c] -> (a, b, c)
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.map (fun line -> line.Trim())
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let isPossible (a, b, c) = 
    a + b > c && a + c > b && b + c > a

let run fileName = 
    let lines = readLines fileName
    let triplets = lines |> List.map parse
    let possible = triplets |> List.filter isPossible |> List.length
    printfn "%d" possible

run "input"
