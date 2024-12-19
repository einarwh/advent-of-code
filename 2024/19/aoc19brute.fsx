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

let buildTrie allTowels =
    let rec loop towels nodes =
        match towels with
        | [] -> nodes
        | t :: rest ->
            nodes |> insertTowel t |> loop rest
    loop allTowels []

let findSubstringIndexes pattern startIndex rootNodes : int list =
    // printfn "findSubstringIndexes for pattern %s with startIndex %d" pattern startIndex
    let rec loop (ix : int) (nodes : Node list) (substringIndexes : int list) =
        if ix < String.length pattern then
            match nodes |> List.tryFind (fun n -> n.c = pattern.[ix]) with
            | Some node ->
                let subs = if node.terminal then (ix - startIndex) :: substringIndexes else substringIndexes
                loop (ix + 1) node.children subs
            | None ->
                substringIndexes
        else substringIndexes
    loop startIndex rootNodes []

// let rec checkPattern (towels : string list) (pattern : string) =
//     if String.length pattern = 0 then true 
//     else 
//         // printfn "checkPattern %A" pattern
//         let candidates = towels |> List.filter (fun t -> pattern.StartsWith(t))
//         // printfn "%A" candidates
//         candidates |> List.exists (fun c -> checkPattern towels (pattern.Substring(String.length c)))

let checkPattern (towels : string list) (pattern : string) (impossible : Set<string>) =
    printfn "checkPattern %s" pattern
    let rec loop (towels : string list) (pattern : string) = 
        if String.length pattern = 0 then 
            true 
        else 
            // printfn "checkPattern %A" pattern
            let candidates = towels |> List.filter (fun t -> pattern.StartsWith(t))
            // printfn "%A" candidates
            candidates |> List.exists (fun c -> loop towels (pattern.Substring(String.length c)))
    loop towels pattern

let solve towels patterns = 
    let rec loop (patterns : string list) (impossible : Set<string>) = 



let run fileName =
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let towels = text.[0] |> split ", " |> Array.toList |> List.sort
    let patterns = text.[1] |> split "\n" |> Array.toList
    let possible = patterns |> List.filter (checkPattern towels) |> List.length
    printfn "%d" possible

    // let lines = readLines fileName
    // lines |> printfn "%A"
    // "brwrr" |> checkPattern towels |> printfn "%A"
    0

run "input"
