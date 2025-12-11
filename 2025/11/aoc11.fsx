// Advent of Code 2025. Day 11: Reactor.
// dotnet fsi aoc11.fsx

open System
open System.IO

let parse (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> (a, b.Split " ")
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let rec solve (device : string) (flow : Map<string, string array>) : int = 
    if device = "out" then 1 
    else 
        let devices = Map.find device flow 
        devices |> Array.sumBy (fun d -> solve d flow)

let run fileName = 
    let lines = readLines fileName
    let map = lines |> Array.map parse |> Map.ofArray
    solve "you" map |> printfn "%d"

run "input.txt"
