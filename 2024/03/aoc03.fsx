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

let rec calc1 (sum : int) (ops : op list) : int = 
    match ops with 
    | [] -> sum 
    | Mul (a, b) :: rest -> 
        calc1 (sum + a*b) rest 
    | _ :: rest -> calc1 sum rest 

let rec calc2 (sum : int) (enabled : bool) (ops : op list) : int = 
    match ops with 
    | [] -> sum 
    | Do :: rest -> calc2 sum true rest 
    | Dont :: rest -> calc2 sum false rest 
    | Mul (a, b) :: rest -> 
        calc2 (if enabled then sum + a*b else sum) enabled rest 

let readLines = 
    File.ReadAllText

let run fileName = 
    let ops = File.ReadAllText fileName |> parse
    calc1 0 ops |> printfn "%d"
    calc2 0 true ops |> printfn "%d"

run "input"
