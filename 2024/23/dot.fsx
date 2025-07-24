// Advent of Code 2018. Day 23.
// dotnet fsi dot.fsx

open System
open System.IO

// graph graphname {
//     a -- b -- c;
//     b -- d;
// }

let parse (s : string) : string*string = 
    let parts = s.Split "-"
    let a = parts[0]
    let b = parts[1]
    if a < b then (a, b) else (b, a)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let edges = lines |> List.map parse |> List.map (fun (a, b) -> sprintf "   %s -- %s;" a b) |> String.concat "\n"
    "graph G {\n" + edges + "\n}" |> printfn "%s"

run "input.txt"
