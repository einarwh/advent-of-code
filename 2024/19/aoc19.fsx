// Advent of Code 2024. Day 19
// dotnet fsi aoc19.fsx

open System
open System.IO

type Node = { c : char; terminal : bool; children : Node list }

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let rec toNode (chars : char list) : Node = 
    match chars with 
    | [] -> failwith "?"
    | [ch] -> { c = ch; terminal = true; children = [] }
    | ch :: rest -> 
        { c = ch; terminal = false; children = [ toNode rest ] }

let rec replace replacement nodes = 
    match nodes with 
    | [] -> [] 
    | n :: rest -> 
        if n.c = replacement.c then 
            replacement :: rest
        else 
            n :: replace replacement rest 

let insertTowel (towel : string) (trie : Node list) = 
    let rec loop towelChars nodes : Node list = 
        match towelChars with 
        | [] -> nodes 
        | c :: restChars -> 
            match nodes |> List.tryFind (fun n -> n.c = c) with 
            | None -> 
                // Insert
                let towelNode = toNode towelChars
                towelNode :: nodes 
            | Some node -> 
                // Keep going 
                replace { node with children = loop restChars node.children } nodes 
    loop (towel |> Seq.toList) trie 

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let towels = text.[0] |> split "\n" |> Array.toList
    let patterns = text.[1] |> split "\n" |> Array.toList
    towels |> printfn "%A"
    patterns |> printfn "%A"
    let nodes = buildTrie towels 
    // let nodes = []
    // let n1 = insertTowel "bwu" nodes
    // printfn "%A" n1
    // let n2 = insertTowel "br" n1 
    // printfn "%A" n2
    


    // let lines = readLines fileName
    // lines |> printfn "%A"

run "sample"
