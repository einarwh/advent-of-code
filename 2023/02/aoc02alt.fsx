// Advent of Code 2023. Day 1: Cube Conundrum
// dotnet fsi aoc02.fsx

open System
open System.IO
open System.Text.RegularExpressions

let getCount (m : Match) : int = 
    m.Groups[1].Value |> int

let readColor (color : string) (s : string) : int  = 
    let pattern = sprintf "(\d+) %s" color
    let matches = Regex.Matches(s, pattern)
    matches |> Seq.map getCount |> Seq.max

let readReds = readColor "red"

let readGreens = readColor "green"

let readBlues = readColor "blue"

let possible (s : string) : int = 
    let reds = s |> readReds  
    let greens = s |> readGreens  
    let blues = s |> readBlues 
    if reds <= 12 && greens <= 13 && blues <= 14 then 
        let pattern = "^Game (\d+)"
        Regex.Match(s, pattern).Groups[1].Value |> int
    else 
        0

let power (s : string) : int = 
    let reds = s |> readReds  
    let greens = s |> readGreens  
    let blues = s |> readBlues 
    reds * greens * blues

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName
    lines |> Array.sumBy possible |> printfn "%d"
    lines |> Array.sumBy power |> printfn "%d"

run "input"
