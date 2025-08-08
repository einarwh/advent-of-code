// Advent of Code 2024. Day 02: Red-Nosed Reports.
// dotnet fsi aoc02.fsx

open System
open System.IO

let parseReport (s : string) = 
    s.Split() |> Array.toList |> List.map int  

let isSafe report = 
    let diffs = report |> List.pairwise |> List.map (fun (a, b) -> a - b)
    let safeIncreasing = diffs |> List.forall (fun d -> d >= 1 && d <= 3)
    let safeDecreasing = diffs |> List.forall (fun d -> d >= -3 && d <= -1)
    safeIncreasing || safeDecreasing

let permute report = 
    let len = report |> List.length 
    let indexedReport = report |> List.indexed
    let keepDifferent i (ix, n) = if i = ix then None else Some n
    [0 .. len - 1] |> List.map (fun i -> indexedReport |> List.choose (keepDifferent i))

let isSafeWithDampener = 
    permute >> List.exists isSafe

let isSafeLenient report = 
    isSafe report || isSafeWithDampener report 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let reports = lines |> List.map parseReport 
    reports |> List.filter isSafe |> List.length |> printfn "%d" 
    reports |> List.filter isSafeLenient |> List.length |> printfn "%d" 

run "input"
