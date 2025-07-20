// Advent of Code 2015. Day 08: Matchsticks.
// dotnet fsi aoc08.fsx

open System
open System.IO

let countCodeLength (s : string) = 
    s.Length 

let countMemoryLength (s : string) = 
    let rec fn count chars = 
        match chars with 
        | [] -> count 
        | '\\' :: '\\' :: rest  
        | '\\' :: '"' :: rest  
        | '\\' :: 'x' :: _ :: _ :: rest 
        | _ :: rest ->
            fn (count + 1) rest
    s |> Seq.toList |> fn 0 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    lines |> printfn "%A"
    let codeLength = lines |> List.map countCodeLength |> List.sum
    let memoryLength = lines |> List.map countMemoryLength |> List.sum
    codeLength - memoryLength |> printfn "%d"

run "sample.txt"
