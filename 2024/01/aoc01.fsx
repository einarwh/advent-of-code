// Advent of Code 2024. Day 01: Historian Hysteria
// dotnet fsi aoc01.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parsePair (line : string) = 
    let pattern = "^(\d+)\s+(\d+)$"
    let m = Regex.Match(line, pattern)
    if m.Success then
        let read (ix : int) = m.Groups.[ix].Value |> int
        Some (read 1, read 2)
    else 
        None

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let part1 fstList sndList = 
    List.zip (fstList |> List.sort) (sndList |> List.sort)
    |> List.map (fun (a, b) -> abs (a - b))
    |> List.sum 

let part2 fstList sndList = 
    let similarity lst n = 
        n * (lst |> List.sumBy (fun m -> if n = m then 1 else 0))
    fstList 
    |> List.map (similarity sndList)
    |> List.sum 

let run fileName = 
    let lines = readLines fileName
    let pairs = lines |> List.choose parsePair
    let fstList = pairs |> List.map fst 
    let sndList = pairs |> List.map snd 
    part1 fstList sndList |> printfn "%d"
    part2 fstList sndList |> printfn "%d"

run "input"
