// Advent of Code 2024. Day 03
// dotnet fsi aoc03.fsx

open System
open System.IO
open System.Text.RegularExpressions

type op = Mul of (int * int) | Do | Dont 

let toOp (m : Match) : op = 
    let name = m.Groups.[1].Value
    if name.StartsWith("don't") then Dont 
    elif name.StartsWith("do") then Do 
    else 
        let values = m.Groups |> Seq.toList |> List.map (fun x -> x.Value)
        let read (ix : int) = m.Groups.[ix].Value |> int
        Mul(read 2, read 3)

let parse (line : string) : op list = 
    let pattern = "(mul\((\d+)\,(\d+)\)|do\(\)|don\'t\(\))"
    let matches = Regex.Matches(line, pattern)
    matches |> Seq.toList |> List.map toOp  

let readLines = 
    File.ReadAllText

let run fileName = 
    let text = File.ReadAllText fileName
    let ops = parse text 
    printfn "%A" ops

    // ops |> List.map (fun (Mul (a, b)) -> a * b) |> List.sum |> printfn "%d"

run "sample2"
