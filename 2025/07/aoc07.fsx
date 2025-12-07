// Advent of Code 2025. Day 07: Laboratories.
// dotnet fsi aoc07.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let splitCount (beam, lines) = 
    let rec loop count beams lines = 
        match lines with 
        | [] -> count 
        | h :: t -> 
            let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
            let collisions = beams |> Set.filter (fun b -> indexes |> List.contains b)
            let splitBeams = collisions |> Set.fold (fun s b -> s |> Set.add (b - 1) |> Set.add (b + 1)) Set.empty
            let beams' = Set.difference beams collisions |> Set.union splitBeams
            let count' = count + Set.count collisions 
            loop count' beams' t
    loop 0 (Set.empty |> Set.add beam) lines 

let timelineCount fn (beam, lines) = 
    match lines with 
    | [] -> 1L
    | h :: t -> 
        let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
        if indexes |> List.contains beam then 
            fn (beam - 1, t) + fn (beam + 1, t) 
        else 
            fn (beam, t) 

let memoize f =
    let cache = System.Collections.Concurrent.ConcurrentDictionary()
    let rec recF x =
        cache.GetOrAdd(x, lazy f recF x).Value
    recF

let solve counter = function 
    | [] -> failwith "?"
    | h :: t -> counter (h |> Seq.findIndex ((=) 'S'), t)

let run fileName = 
    let lines = readLines fileName
    lines |> solve splitCount |> printfn "%d"
    lines |> solve (memoize timelineCount) |> printfn "%d"

run "input.txt"
