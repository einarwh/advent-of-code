// Advent of Code 2015. Day 16: Aunt Sue.
// dotnet fsi aoc16.fsx

open System
open System.IO

let clues = [
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

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let sues = lines |> List.map parse 
    let correctSue = 
        sues |> List.filter (fun (n, facts) -> facts |> List.forall (fun f -> List.contains f clues)) |> List.head |> fst
    printfn "%d" correctSue

run "input.txt"
