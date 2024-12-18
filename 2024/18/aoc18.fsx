// Advent of Code 2024. Day 18: RAM Run.
// dotnet fsi aoc18.fsx

open System
open System.IO

type Pos = (int*int)

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let join (sep : string) (seq : string seq) = String.Join(sep, seq)

let charsToString (chars : char array) = new String(chars)

let parsePos (s : string) = 
    match s |> split "," |> Array.map int with 
    | [|a; b|] -> Some (a, b)
    | _ -> None 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let getAllPositions dim = 
    let rowCount = dim 
    let colCount = dim 
    [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]

let visualize dim corrupted =
    let range = [ 0 .. dim ]
    let createRow y = 
        range 
        |> List.map (fun x -> if Set.contains (x, y) corrupted then '#' else '.')
        |> List.toArray 
        |> charsToString
    range
    |> List.map (createRow)
    |> join "\n"
    |> printfn "%s"

let solve dim corrupted = 
    let startPos = (0, 0)
    let endPos = (dim, dim)
    0

let run dim byteCount fileName = 
    let lines = readLines fileName
    let corrupted = lines |> List.choose parsePos |> List.take byteCount |> Set.ofList
    corrupted |> printfn "%A"
    visualize dim corrupted

run 6 12 "sample"
// run 70 1024 "input"
