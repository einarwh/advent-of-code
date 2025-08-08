// Advent of Code 2020. Day 6: Custom Customs.
// dotnet fsi aoc06.fsx

open System.IO

let anyone (s : string) : int =
    s.Replace("\n", "") |> Seq.distinct |> Seq.length

let everyone (s : string) : int =
    s.Split("\n")
    |> Array.map Set.ofSeq
    |> Array.reduce Set.intersect 
    |> Set.count

let countBy (counter : string -> int) =
    Array.map counter >> Array.sum >> printfn "%d"

let run (text : string) =
    let groups = text.Trim().Split("\n\n")
    groups |> countBy anyone 
    groups |> countBy everyone 

"input.txt" |> File.ReadAllText |> run 
