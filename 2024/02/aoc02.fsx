// Advent of Code 2024. Day 02
// dotnet fsi aoc02.fsx

open System
open System.IO

let parseReport (s : string) : int list = 
    s.Split() |> Array.toList |> List.map int  

let isSafe (report : int list) : bool = 
    let diffs = report |> List.pairwise |> List.map (fun (a, b) -> a - b)
    let safeIncreasing = diffs |> List.forall (fun d -> d >= 1 && d <= 3)
    let safeDecreasing = diffs |> List.forall (fun d -> d >= -3 && d <= -1)
    safeIncreasing || safeDecreasing

let permute (report : int list) : int list list = 
    let len = report |> List.length 
    let indexes = [0 .. len - 1]
    let indexedReport = report |> List.indexed
    indexes |> List.map (fun i -> indexedReport |> List.choose (fun (ix, n) -> if i = ix then None else Some n))

let isSafeWithDampener (report : int list) : bool = 
    report |> permute |> List.exists isSafe

let isSafeLenient (report : int list) : bool = 
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
