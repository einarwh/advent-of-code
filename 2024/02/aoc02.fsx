// Advent of Code 2024. Day 02
// dotnet fsi aoc02.fsx

open System
open System.IO

let parseReport (s : string) : int array = 
    s.Split() |> Array.map int  

let isSafe (report : int array) : bool = 
    let diffs = report |> Array.pairwise |> Array.map (fun (a, b) -> a - b)
    let safeIncreasing = diffs |> Array.forall (fun d -> d >= 1 && d <= 3)
    let safeDecreasing = diffs |> Array.forall (fun d -> d >= -3 && d <= -1)
    safeIncreasing || safeDecreasing

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let reports = lines |> List.map parseReport 
    reports |> List.filter isSafe |> List.length |> printfn "%d" 

run "input"
