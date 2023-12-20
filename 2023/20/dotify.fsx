// Advent of Code 2023. Day 20: ?
// dotnet fsi aoc20.fsx

open System
open System.IO

type Node = 
    | Broadcaster 
    | FlipFlop  
    | Conjunction  

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let parseLine (s : string) = 
    let splat = s.Split(" -> ")
    let str = splat[0]
    let targets = splat[1].Split(", ") |> Array.toList
    if str = "broadcaster" then 
        (Broadcaster, str, targets)
    else 
        let kind = if str[0] = '&' then Conjunction else FlipFlop
        let label = str.Substring(1)
        (kind, label, targets)

let toEdgeLine (_, label, targets) = 
    let ts = 
        match targets with 
        | [ t ] -> t 
        | _ -> sprintf "{ %s }" (targets |> String.concat " ") 
    sprintf "    %s -> %s" label ts

let toConjunctionLine label = 
    sprintf "    %s [ shape = box ]" label

let run inFile outFile =
    let lines = readLines inFile
    let edges = lines |> Array.map parseLine
    let conjs = edges |> Array.choose (fun (k, src, _) -> if k = Conjunction then Some src else None)
    let conjLines = conjs |> Array.map toConjunctionLine
    let edgeLines = edges |> Array.map (toEdgeLine)
    let before = [| 
        "digraph {"
        "    node [ shape = circle ]"
        "    button [ shape = point ]"
        "    broadcaster [ shape = diamond ]"
    |]
    let buttonEdge = [|
        "    button -> broadcaster"
    |]
    let after = [|
        "}"
    |]
    let lines = [ before; conjLines; buttonEdge; edgeLines; after ] |> Array.concat 
    File.WriteAllLines(outFile, lines)


let args = fsi.CommandLineArgs |> Array.tail
let inputFileName = args |> Array.item 0
let outputFileName = args |> Array.tryItem 1 |> Option.defaultValue (sprintf "%s.dot" inputFileName)

run inputFileName outputFileName