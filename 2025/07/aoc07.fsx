// Advent of Code 2025. Day 07: Laboratories.
// dotnet fsi aoc07.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

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

let rec timelineCount (mem : Map<int * int, int64>) (beam : int, depth : int) (lines : string list) = 
    if Map.containsKey (beam, depth) mem then 
        mem, Map.find (beam, depth) mem
    else 
        match lines with 
        | [] -> mem, 1L
        | h :: t -> 
            let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
            if indexes |> List.contains beam then 
                let memL, countL = timelineCount mem (beam - 1, depth + 1) t 
                let memR, countR = timelineCount memL (beam + 1, depth + 1) t 
                let count = countL + countR 
                let mem' = memR |> Map.add (beam, depth) count  
                mem', count 
            else 
                timelineCount mem (beam, depth + 1) t 

let timelines (lines : string list) = 
    match lines with 
    | [] -> failwith "?"
    | h :: t -> 
        let ix = h |> Seq.findIndex ((=) 'S')
        timelineCount Map.empty (ix, 1) t |> snd 

let run fileName = 
    let lines = readLines fileName
    lines |> split |> printfn "%d"
    lines |> timelines |> printfn "%d"

run "input.txt"
