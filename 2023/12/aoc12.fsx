// Advent of Code 2023. Day 12: Hot Springs
// dotnet fsi aoc12.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parseLine (s : string) = 
    let parts = s.Split(" ")
    let springs = parts[0]
    let damaged = parts[1]
    (springs |> Seq.toList, damaged.Split(",") |> Array.toList |> List.map int)

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let rec arrangements (springs : char list) : char list list=
    match springs with 
    | [] -> [[]]
    | s :: t -> 
        let rest = arrangements t 
        match s with 
        | '?' -> 
            let broken = rest |> List.map (fun r -> '#' :: r)
            let working = rest |> List.map (fun r -> '.' :: r)
            broken @ working
        | _ -> 
            rest |> List.map (fun r -> s :: r)

let countDamaged (s : string) = 
    Regex.Matches(s, "\#+") 
    |> Seq.map (fun m -> m.Value |> String.length)
    |> Seq.toList

let check (pattern : int list) (springs : string) =
    let damaged = countDamaged springs
    if List.length damaged = List.length pattern then 
        List.zip damaged pattern 
        |> List.forall (fun (a, b) -> a = b)
    else false 

let countArrangements (line : string) = 
    let springs, damaged = line |> parseLine
    springs 
    |> Seq.toList 
    |> arrangements
    |> List.map (List.toArray >> String)
    |> List.filter (check damaged)
    |> List.length

let run fileName = 
    let lines = readLines fileName |> Array.toList
    lines 
    |> List.map countArrangements
    |> List.sum 
    |> printfn "%d"
    
"input" |> run 
