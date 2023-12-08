// Advent of Code 2023. 
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
    File.ReadAllLines
    >> Array.filter ((<>) String.Empty)

let rec solve steps start stop instructions (map : Map<string, string*string>) = 
    if start = stop then steps
    else 
        let ins = instructions |> Seq.head 
        let tail = instructions |> Seq.tail
        let (left, right) = map[start]
        let next = 
            match ins with 
            | 'R' -> right 
            | 'L' -> left 
            | _ -> failwith <| sprintf "No instruction %c" ins
        solve (steps + 1) next stop tail map

let run fileName = 
    let lines = readLines fileName |> Array.toList
    match lines with 
    | h :: t -> 
        let ins = h |> Seq.repeat 
        let map = t |> List.map parse |> Map.ofList
        solve 0 "AAA" "ZZZ" ins map |> printfn "%d"
    | _ -> failwith "Wrong"

"input" |> run 
