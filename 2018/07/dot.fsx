// Advent of Code 2018. Day 07
// dotnet fsi aoc07.fsx

open System
open System.IO

let parse (s : string) = 
    let a = s.Split(" ")
    a[1], a[7]

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let edges = lines |> List.map parse |> List.map (fun (src, tgt) -> sprintf "   %s -> %s;" src tgt) |> String.concat "\n"
    "digraph G {\n" + edges + "\n}" |> printfn "%s"

run "input.txt"
