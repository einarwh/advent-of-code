// Advent of Code 2023. Day 9: Mirage Maintenance.
// dotnet fsi aoc09.fsx

open System
open System.IO

let parseLine (s : string) = 
    s.Split(" ") |> Array.toList |> List.map int64

let differences = 
    List.pairwise >> List.map (fun (a, b) -> (b - a)) 

let rec back p numbers = 
    match numbers with 
    | [] -> []
    | n :: t -> 
        let d = n - p
        d :: back d t

let histories numbers = 
    let rec loop numbers firsts lasts = 
        if List.forall (fun n -> n = 0L) numbers then 
            let forwards = List.sum lasts
            let backwards = firsts |> back 0L |> List.last
            (forwards, backwards)
        else 
            let next = differences numbers 
            let first = List.head numbers
            let last = List.last numbers
            loop next (first :: firsts) (last :: lasts)
    loop numbers [] []

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName = 
    let parsed = readLines fileName |> Array.toList |> List.map parseLine
    let historiesList = parsed |> List.map histories
    let forwards = historiesList |> List.map fst 
    let backwards = historiesList |> List.map snd
    forwards |> List.sum |> printfn "%d"
    backwards |> List.sum |> printfn "%d"

"input.txt" |> run 
