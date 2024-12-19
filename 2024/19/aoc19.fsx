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

let checkPattern rootNodes (pattern : string) = 
    printfn "\ncheckPattern: %A" pattern
    let rec loop prevIx ix = 
        if ix >= String.length pattern then 
            // printfn "pattern done!"
            true 
        else 
            printfn "ix: %d - find match for pattern %A" ix (pattern.Substring(ix)) 
            let indexes = findSubstringIndexes pattern ix rootNodes 
            indexes |> printfn "substring indexes at %d: %A" ix
            indexes |> List.map (fun i -> pattern.Substring(ix, i + 1)) |> printfn "%A" 
            
            if indexes |> List.isEmpty then 
                false
            else 
                indexes |> List.exists (fun i -> loop ix (ix + i + 1))
    loop 0 0

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let towels = text.[0] |> split ", " |> Array.toList
    let patterns = text.[1] |> split "\n" |> Array.toList
    towels |> printfn "%A"
    patterns |> printfn "%A"
    let rootNodes = buildTrie towels 
    printfn "%A" rootNodes
    // let nodes = []
    // let n1 = insertTowel "bwu" nodes
    // printfn "%A" n1
    // let n2 = insertTowel "br" n1 
    // printfn "%A" n2
    // let pattern1 = "brwrr"
    // let foo = findSubstringIndexes pattern1 0 rootNodes
    // printfn "%A" foo
    // "brwrr" |> checkPattern rootNodes |> printfn "%A"
    let check pattern = 
        let result = pattern |> checkPattern rootNodes 
        printfn "pattern %A: %b" pattern result
    "brwrr" |> check
    "bggr" |> check
    "gbbr" |> check
    "rrbgbr" |> check
    "ubwu" |> check
    "bwurrg" |> check
    "brgr" |> check
    "bbrgwb" |> check


    // let lines = readLines fileName
    // lines |> printfn "%A"
    0

run "sample"
