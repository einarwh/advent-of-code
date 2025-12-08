// Advent of Code 2025. Day 07: Laboratories.
// dotnet fsi aoc07.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

type SplitState = 
    { beams : Set<int>
      depth : int 
      lines : string list
      seen : Set<int*int>
      count : int }

let splitCount (state : SplitState) = 
    match state.lines with 
    | [] -> state 
    | h :: t -> 
        let beams = state.beams 
        let depth = state.depth 
        let count = state.count
        let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
        let collisions = beams |> Set.filter (fun b -> indexes |> List.contains b)
        let splitBeams = collisions |> Set.fold (fun s b -> s |> Set.add (b - 1) |> Set.add (b + 1)) Set.empty
        let beams' = Set.difference beams collisions |> Set.union splitBeams
        let count' = count + Set.count collisions 
        let seen = beams |> Set.map (fun b -> b, depth) |> Set.union state.seen
        { beams = beams'; depth = depth + 1; lines = t; seen = seen; count = count' }

let rec splitLoop (state : SplitState) = 
    match state.lines with 
    | [] -> state 
    | _ -> 
        state |> splitCount |> splitLoop 

let split (lines : string list) = 
    match lines with 
    | [] -> failwith "?"
    | h :: t -> 
        let ix = h |> Seq.findIndex ((=) 'S')
        let beams = Set.empty |> Set.add ix
        let state = 
            { beams = beams 
              depth = 1 
              lines = t
              seen = Set.empty 
              count = 0 }
        let state' = splitLoop state 
        state'.count

type CallInfo = 
    { pos : int*int 
      lines : string list }

type StackItem = 
    Left of CallInfo | Right of CallInfo | Combine | Down of CallInfo | Bubble

type TimelineState = 
    { mem : Map<int * int, int64> 
      pos : int*int
      lines : string list
      seen : Set<int*int>
      count : int64
      stack : StackItem list }

let timelineStep (state : TimelineState) : TimelineState =
    let mem = state.mem 
    let beam, depth = state.pos 
    if Map.containsKey (beam, depth) mem then 
        let count = Map.find (beam, depth) mem
        { state with count = state.count + count }
    else 
        let lines = state.lines 
        match lines with 
        | [] ->
            let mem' = state.mem |> Map.add (beam, depth) 1L
            { state with count = state.count + 1L; mem = mem' }
        | h :: t -> 
            let indexes = h |> Seq.toList |> List.indexed |> List.choose (fun (i, c) -> if c = '^' then Some i else None)
            if indexes |> List.contains beam then 
                let seen = state.seen |> Set.add (beam, depth) |> Set.add (beam - 1, depth)
                let itemL = Left { pos = beam - 1, depth + 1; lines = t }
                let itemR = Right { pos = beam + 1, depth + 1; lines = t }
                let stack = itemL :: itemR :: Combine :: state.stack 
                { state with seen = seen; stack = stack }
            else 
                let seen = state.seen |> Set.add (beam, depth)
                let item = Down { pos = beam, depth + 1; lines = t }
                let stack = item :: Bubble :: state.stack 
                { state with seen = seen; pos = beam, depth + 1; stack = stack }

let rec timelineLoop (state : TimelineState) : TimelineState =
    let state' = 
        match state.stack with 
        | [] -> 
            timelineStep state 
        | item :: rest -> 
            match item with 
            | Left info -> 
                let state' = { state with pos = info.pos; lines = info.lines; stack = rest }
                timelineStep state' 
            | Right info -> 
                let state' = { state with pos = info.pos; lines = info.lines; stack = rest }
                timelineStep state' 
            | Down info -> 
                let state' = { state with pos = info.pos; lines = info.lines; stack = rest }
                timelineStep state' 
            | Combine -> 
                let (beam, depth) = state.pos
                let b, d = beam - 1, depth - 1
                let pos = (b, d)
                let countL = Map.find (b - 1, d + 1) state.mem
                let countR = Map.find (b + 1, d + 1) state.mem 
                let mem' = state.mem |> Map.add pos (countL + countR)
                { state with mem = mem'; stack = rest; pos = pos } 
            | Bubble -> 
                let (beam, depth) = state.pos 
                let pos = beam, depth - 1
                let countD = Map.find (beam, depth) state.mem
                let mem' = state.mem |> Map.add pos countD
                { state with mem = mem'; stack = rest; pos = pos } 
    // printfn "%d" state'.count
    if state'.stack |> List.isEmpty then state' 
        else timelineLoop state'
    
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
        let result = timelineLoop state
        result.count

let run fileName = 
    let lines = readLines fileName
    lines |> split |> printfn "%d"
    lines |> timelines |> printfn "%d"

run "input.txt"
