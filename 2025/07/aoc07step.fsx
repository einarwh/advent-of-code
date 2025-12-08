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

type CallInfo = 
    { pos : int*int 
      lines : string list }

type StackItem = 
    Call of CallInfo | Combine 

type TimelineState = 
    { mem : Map<int * int, int64> 
      pos : int*int
      lines : string list
      seen : Set<int*int>
      count : int64
      stack : StackItem list }

let timelineStep (state : TimelineState) : TimelineState =
    printfn "timelineStep"
    printfn " - pos: %A" state.pos
    printfn " - count: %A" state.count
    let mem = state.mem 
    let beam, depth = state.pos 
    if Map.containsKey (beam, depth) mem then 
        let count = Map.find (beam, depth) mem
        { state with count = state.count + count }
    else 
        let lines = state.lines 
        match lines with 
        | [] -> { state with count = state.count + 1L }
        | h :: t -> 
            printfn "line %A" h
            let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
            if indexes |> List.contains beam then 
                let seen = state.seen |> Set.add (beam, depth) |> Set.add (beam - 1, depth)
                let itemL = Call { pos = beam - 1, depth + 1; lines = t }
                let itemR = Call { pos = beam + 1, depth + 1; lines = t }
                let stack = itemL :: itemR :: Combine :: state.stack 
                { state with seen = seen; stack = stack }
            else 
                let seen = state.seen |> Set.add (beam, depth)
                let item = Call { pos = beam, depth + 1; lines = t }
                let stack = item :: state.stack 
                { state with seen = seen; pos = beam, depth + 1; stack = stack }

let rec timelineLoop (state : TimelineState) : TimelineState =
    printfn "\ntimelineLoop %A" state.pos
    let state' = 
        match state.stack with 
        | [] -> 
            printfn "call stack empty"
            let state' = timelineStep state 
            printfn "after step %A" state' 
            state' 
        | item :: rest -> 
            match item with 
            | Call info -> 
                printfn "CALL"
                let state' = { state with pos = info.pos; lines = info.lines; stack = rest }
                timelineStep state' 
            | Combine -> 
                printfn "COMBINE"
                let mem' = state.mem |> Map.add state.pos state.count
                { state with mem = mem'; stack = rest } 
    if state'.stack |> List.isEmpty then state' 
        else timelineLoop state'
    

// type TimelineState = 
//     { mem : Map<int * int, int64> 
//       pos : int*int
//       lines : string list
//       seen : Set<int*int>
//       count : int64
//       stack : StackItem list }


let timelines (lines : string list) = 
    match lines with 
    | [] -> failwith "?"
    | h :: t -> 
        let ix = h |> Seq.findIndex ((=) 'S')
        let state = 
            { mem = Map.empty  
              pos = ix, 1
              lines = t 
              seen = Set.empty 
              count = 0L 
              stack = [] } 
        timelineLoop state

let run fileName = 
    let lines = readLines fileName
    // lines |> split |> printfn "%d"
    lines |> timelines |> printfn "%A"

run "sample.txt"
