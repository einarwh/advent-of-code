// Advent of Code 2023. Day 8: Haunted Wasteland
// dotnet fsi aoc08.fsx

open System
open System.IO

module Seq = 
  let repeat items = 
    seq { while true do yield! items }

let parse (s : string) = 
    let a = s.Substring(0, 3)
    let b = s.Substring(7, 3)
    let c = s.Substring(12, 3)
    (a, (b, c))

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let solve (map : Map<string, string*string>) (instructions : char seq) stopCheck start = 
    let timestamp = DateTime.Now
    let rec loop steps instructions pos = 
        if (steps % 100 = 0) then 
            let elapsedSeconds = int <| (DateTime.Now - timestamp).TotalSeconds
            printfn "%d steps in %d seconds" steps elapsedSeconds
        if stopCheck pos then steps
        else 
            let (left, right) = map[pos]
            let ins = Seq.head instructions
            let instructions' = Seq.tail instructions
            let next = 
                match ins with 
                | 'R' -> right 
                | 'L' -> left 
                | _ -> failwith "Wrong"
            loop (steps + 1) instructions' next 
    loop 0 instructions start

let lcm x y =
    let rec gcd (x : int64) (y : int64) = 
        if y = 0 then abs x else gcd y (x % y)
    x * y / (gcd x y)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    match lines with 
    | h :: t -> 
        let map = t |> List.map parse |> Map.ofList
        let infinite = Seq.repeat h
        solve map infinite ((=) "ZZZ") "AAA" |> printfn "%d"
        // let isStartNode (s : string) = s.EndsWith('A')
        // let isEndNode (s : string) = s.EndsWith('Z')
        // map.Keys 
        // |> Seq.filter isStartNode
        // |> Seq.map (int64 << solve map h isEndNode)
        // |> Seq.reduce lcm
        // |> printfn "%d"
        "."
    | _ -> failwith "Wrong"

"input" |> run 
