// Advent of Code 2024. Day 03
// dotnet fsi aoc03.fsx

open System
open System.IO
open System.Text.RegularExpressions

let mul (m : Match) : int = 
    let read (ix : int) = m.Groups.[ix].Value |> int
    read 1 * read 2

let parse (line : string) = 
    let pattern = "mul\((\d+),(\d+)\)"
    let matches = Regex.Matches(line, pattern)
    matches |> Seq.map mul |> Seq.sum 

let readLines = 
    File.ReadAllText

let run fileName = 
    let text = File.ReadAllText fileName
    let foo = parse text 
    foo |> printfn "%A"

run "input"
