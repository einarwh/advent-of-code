// Advent of Code 2015. Day 16: Aunt Sue.
// dotnet fsi aoc16.fsx

open System
open System.IO

let readings = [
    ("children", 3)
    ("cats", 7)
    ("samoyeds", 2)
    ("pomeranians", 3)
    ("akitas", 0)
    ("vizslas", 0)
    ("goldfish", 5)
    ("trees", 3)
    ("cars", 2)
    ("perfumes", 1)
] 

let parse (s : string) = 
    let colonIndex = s.IndexOf(':')
    let sueStr = s.Substring(0, colonIndex)
    let restStr = s.Substring(colonIndex + 2)
    let sueNumber = int <| sueStr.Split(" ")[1]
    let toFact (str : string) = 
        let parts = str.Split(": ")
        parts[0], int parts[1]
    let facts = restStr.Split(", ") |> Array.map toFact |> Array.toList
    sueNumber, facts

let checkFact1 fact = 
    readings |> List.contains fact

let checkFact2 (prop : string, value : int) : bool = 
    let reading = readings |> List.find (fun (p, _) -> p = prop) |> snd
    match prop with 
    | "cats"
    | "trees" -> value > reading 
    | "pomeranians" 
    | "goldfish" -> value < reading 
    | _ -> value = reading

let findSue checkFact (sues : (int * (string*int) list) list) : int = 
    sues |> List.filter (fun (_, facts) -> facts |> List.forall checkFact) |> List.head |> fst

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let sues = lines |> List.map parse 
    sues |> findSue checkFact1 |> printfn "%d"
    sues |> findSue checkFact2 |> printfn "%d"

run "input.txt"
