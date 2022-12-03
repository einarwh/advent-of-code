// Advent of Code 2022. 
// Day 3: Rucksack Reorganization, Part A.
// dotnet fsi aoc03a.fsx

open System.IO 

let compartmentalize (items : string) = 
    let half = items.Length / 2
    items.Substring(0, half), items.Substring(half, half)

let translateCharCode (charCode : int) =
    if charCode >= 97 then charCode - 97 + 1 
    else charCode - 65 + 27

let toPriority (ch : char) =
    ch |> int |> translateCharCode

let findItem (items : string) = 
    let (first, second) = compartmentalize items
    let itemSet = Set.intersect (Set.ofSeq first)  (Set.ofSeq second)
    let ch = itemSet |> Set.toList |> List.head
    ch |> toPriority

let run lines = 
    // Part A
    lines 
    |> Array.map findItem
    |> Array.sum
    |> printfn "Part A: %A"
    // Part B
    lines 
    |> Array.map Set.ofSeq
    |> Array.chunkBySize 3
    |> Array.sumBy (Set.intersectMany >> Set.toList >> List.head >> toPriority)
    |> printfn "Part B: %A"

"input"
|> File.ReadAllLines 
|> Array.filter (fun s -> s.Length > 0)
|> run