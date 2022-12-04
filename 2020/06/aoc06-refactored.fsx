// Advent of Code 2020. Day 6.
// dotnet fsi aoc06-refactored.fsx

open System.IO

let countGroup reducer =
    Array.map Set.ofSeq >> Array.reduce reducer >> Set.count
    
let any = countGroup Set.union

let all = countGroup Set.intersect

let count counter =
    Array.map counter >> Array.sum >> printfn "%d"

let run (text : string) =
    let groups = text.Trim().Split("\n\n") |> Array.map (fun s -> s.Split("\n"))
    groups |> count any
    groups |> count all 

"input" |> File.ReadAllText |> run 
