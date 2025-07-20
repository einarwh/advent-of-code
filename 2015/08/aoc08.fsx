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
    let withoutQuotes = s.Substring(1, s.Length - 2)
    withoutQuotes |> Seq.toList |> fn 0 

let encode (s : string) = 
    let rec fn chars = 
        match chars with 
        | [] -> []
        | '\\' :: rest -> '\\' :: '\\' :: fn rest
        | '"' :: rest -> '\\' :: '"' :: fn rest
        | c :: rest -> c :: fn rest 
    let content = s |> Seq.toList |> fn |> List.toArray |> fun cs -> new string(cs)
    "\"" + content + "\""

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let codeLength = lines |> List.map countCodeLength |> List.sum
    let memoryLength = lines |> List.map countMemoryLength |> List.sum
    codeLength - memoryLength |> printfn "%d"
    let encodedLength = lines |> List.map encode |> List.map countCodeLength |> List.sum 
    encodedLength - codeLength |> printfn "%d"
    
run "input.txt"
