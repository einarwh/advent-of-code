// Advent of Code 2024. Day 03
// dotnet fsi aoc03.fsx

open System
open System.IO
open System.Text.RegularExpressions

type op = 
    | Mul of (int * int)
    | Do 
    | Dont 

let mul (m : Match) : op = 
    let read (ix : int) = m.Groups.[ix].Value |> int
    Mul (read 1, read 2)

let toOp (m : Match) : op = 
    let name = m.Groups.[1].Value
    printfn "%A" name
    Do 

let parse (line : string) : op list = 
    let pattern = "((mul)\((\d+)\,(\d+)\)|(do)\(\)|(don\'t)\(\))"
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
