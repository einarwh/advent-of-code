// Advent of Code 2017. Day 13: Packet Scanners.
// dotnet fsi aoc13.fsx

open System
open System.IO

let readLine (s : string) = 
    match s.Split([|": "|], StringSplitOptions.None) with 
    | [|a; b|] -> Some (int a, int b)
    | _ -> None

let severity = 
    List.fold (fun x (i, d) -> x + if i % (2 * (d - 1)) = 0 then i * d else 0) 0 

let pass wait = 
    List.forall (fun (i, d) -> d = 0 || (wait + i) % (2 * (d - 1)) > 0)

let rec solve wait vals = 
    if pass wait vals then wait else solve (wait + 1) vals

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let pairs = lines |> List.choose readLine
    pairs |> severity |> printfn "%d"
    pairs |> solve 0 |> printfn "%d"

run "input.txt"
