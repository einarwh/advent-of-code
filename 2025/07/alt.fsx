// Advent of Code 2025. Day 07: Laboratories.
// dotnet fsi aoc07.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let memoizeRec f =
    let cache = System.Collections.Concurrent.ConcurrentDictionary()
    let rec recF x =
        cache.GetOrAdd(x, lazy f recF x).Value
    recF

let split (lines : string list) = 
    let rec loop (count : int) (beams : Set<int>) (lines : string list) = 
        match lines with 
        | [] -> count 
        | h :: t -> 
            let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
            let collisions = beams |> Set.filter (fun b -> indexes |> List.contains b)
            let splitBeams = collisions |> Set.fold (fun s b -> s |> Set.add (b - 1) |> Set.add (b + 1)) Set.empty
            let beams' = Set.difference beams collisions |> Set.union splitBeams
            let count' = count + Set.count collisions 
            loop count' beams' t
    match lines with 
    | [] -> failwith "?"
    | h :: t -> 
        let ix = h |> Seq.findIndex ((=) 'S')
        let beams = Set.empty |> Set.add ix
        loop 0 beams t 

let rec timelineCount fn (beam : int, lines : string list) = 
    match lines with 
    | [] -> 1L
    | h :: t -> 
        let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
        if indexes |> List.contains beam then 
            let countL = fn (beam - 1, t) 
            let countR = fn (beam + 1, t) 
            let count = countL + countR 
            count 
        else 
            fn (beam, t) 

let timelines (lines : string list) = 
    match lines with 
    | [] -> failwith "?"
    | h :: t -> 
        let ix = h |> Seq.findIndex ((=) 'S')
        let fn = memoizeRec <| timelineCount 
        fn (ix, t) 

let run fileName = 
    let lines = readLines fileName
    lines |> split |> printfn "%d"
    lines |> timelines |> printfn "%d"

run "input.txt"
