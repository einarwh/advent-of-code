// Advent of Code 2018. Day 2: Inventory Management System
// dotnet fsi aoc02.fsx

open System
open System.IO

let parseLine (s : string) = 
    s |> Seq.toList |> List.countBy id |> List.map snd

let diff (s1 : string) (s2 : string) = 
    Seq.zip s1 s2 
    |> Seq.filter (fun (c1, c2) -> c1 <> c2)
    |> Seq.length

let prototypes a b = 
    1 = diff a b

let rec solve all remaining = 
    match remaining with 
    | [] -> failwith "not found"
    | h :: t ->
        match all |> List.tryFind (prototypes h) with 
        | Some box ->
            Seq.zip h box 
            |> Seq.filter (fun (a, b) -> a = b)
            |> Seq.map fst 
            |> Seq.toArray
            |> String
        | None -> solve all t

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName = 
    let boxes = readLines fileName |> Array.toList
    let lists = boxes |> List.map parseLine
    let count n = List.filter (List.contains n) >> List.length
    let twos = lists |> count 2
    let threes = lists |> count 3
    twos * threes |> printfn "%d"
    solve boxes boxes |> printfn "%s"

"input.txt" |> run 
