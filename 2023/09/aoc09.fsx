// Advent of Code 2023. Day 9: Mirage Maintenance
// dotnet fsi aoc09.fsx

open System
open System.IO

let parseLine (s : string) = 
    s.Split(" ") |> Array.toList |> List.map int64

let differences numbers = 
    numbers |> List.pairwise |> List.map (fun (a, b) -> (b - a)) 

let historyValue numbers = 
    let rec loop numbers lasts = 
        if List.forall (fun n -> n = 0L) numbers then 
            List.sum lasts
        else 
            let diffs = differences numbers 
            let last = List.last numbers
            loop diffs (last :: lasts)
    loop numbers []

let rec calculate p numbers : int64 list = 
    match numbers with 
    | [] -> []
    | n :: t -> 
        let d = n - p
        d :: calculate d t

let backwardsHistory numbers = 
    let rec loop numbers (firsts : int64 list) = 
        if List.forall (fun n -> n = 0L) numbers then 
            firsts |> (calculate 0L) |> List.last
        else 
            let diffs = differences numbers 
            let first = List.head numbers
            loop diffs (first :: firsts)
    loop numbers []

let histories numbers = 
    let rec loop numbers firsts lasts = 
        if List.forall (fun n -> n = 0L) numbers then 
            let forwards = List.sum lasts
            let backwards = firsts |> calculate 0L |> List.last
            (forwards, backwards)
        else 
            let diffs = differences numbers 
            let first = List.head numbers
            let last = List.last numbers
            loop diffs (first :: firsts) (last :: lasts)
    loop numbers [] []

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName = 
    let parsed = readLines fileName |> Array.toList |> List.map parseLine
    parsed 
    |> List.sumBy historyValue
    |> printfn "%d"

    parsed 
    |> List.sumBy backwardsHistory
    |> printfn "%A"

    let historiesList = parsed |> List.map histories
    let forwards = historiesList |> List.map fst 
    let backwards = historiesList |> List.map snd
    forwards |> List.sum |> printfn "%d"
    backwards |> List.sum |> printfn "%d"

    // parsed 
    // |> List.map backwardsHistory
    // |> printfn "%A"


"input" |> run 
