// Advent of Code 2023. Day 4: Scratchcards
// dotnet fsi aoc04.fsx

open System
open System.IO

let split (sep : string) (s : string) = s.Split(sep)

let trim (s : string) = s.Trim()

let isNonEmpty (s : string) = s.Length > 0  

let parseLine (line : string) : int list = 
    let s1 = split ": " line 
    let s2 = split " | " s1[1]
    let ws = s2[0]
    let cs = s2[1]
    let winning = ws |> split " " |> Array.filter isNonEmpty |> Array.map int 
    let cards = cs |> split " " |> Array.filter isNonEmpty |> Array.map int  
    let found = Set.intersect (Set.ofArray winning) (Set.ofArray cards) |> Set.toList 
    found

let calculate (cards : int list) : int = 
    match List.length cards with 
    | 0 -> 0 
    | n -> pown 2 (n - 1)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    lines 
    |> List.map parseLine 
    |> List.sumBy calculate 
    |> printfn "%d"

"input" |> run 
