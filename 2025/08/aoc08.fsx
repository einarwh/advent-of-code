// Advent of Code 2025. Day 08.
// dotnet fsi aoc08.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList
    
let parse (s : string) = 
    match s.Split "," |> Array.map int with 
    | [|a; b; c|] -> (a, b, c)
    | _ -> failwith "?"

let distance (x1, y1, z1) (x2, y2, z2) = 
    let dx, dy, dz = x2-x1, y2-y1, z2-z1 
    let sq n = n * n |> float
    sqrt <| sq dx + sq dy + sq dz 

let run fileName = 
    let lines = readLines fileName
    let boxes = lines |> List.map parse
    boxes |> printfn "%A"

run "sample.txt"
