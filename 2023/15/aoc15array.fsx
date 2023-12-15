// Advent of Code 2023. Day 15: Lens Library
// dotnet fsi aoc15.fsx

open System
open System.IO

type Lens = (string * int)

type Op = 
    | Insert of Lens
    | Remove of string

let readText fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd()

let parseOp (s : string) = 
    if s.EndsWith('-') then 
        let label = s.Substring(0, s.Length - 1)
        Remove label
    else 
        let ss = s.Split("=")
        let label = ss[0] 
        let lens = int ss[1]
        Insert (label, lens)

let parseAllOps (s : string) = 
    s.Split(",") |> Array.toList |> List.map parseOp

let getHash s = 
    let folder current ch  = 
        ((current + int ch) * 17) % 256
    s |> Seq.toList |> List.fold folder 0

let part1 (input : string) = 
    input.Split(",") |> Array.sumBy getHash

let executeOp (hashmap : Lens list array) (op : Op) = 
    let contains label = 
        List.exists (fun (l, _) -> l = label)
    let remove label = 
        List.filter (fun (l, _) -> l <> label)
    let replace label focal = 
        List.map (fun (l, f) -> if l = label then (l, focal) else (l, f))
    let append label focal = 
        (@) [label, focal]
    match op with 
    | Remove label -> 
        let hash = getHash label 
        hashmap[hash] <- hashmap[hash] |> remove label
    | Insert (label, focal) -> 
        let hash = getHash label 
        let lenses = hashmap[hash]
        if lenses |> contains label then 
            hashmap[hash] <- lenses |> replace label focal
        else 
            hashmap[hash] <- lenses |> append label focal

let rec execute ops hashmap = 
    match ops with 
    | [] -> () 
    | op :: rest -> 
        executeOp hashmap op
        execute rest hashmap

let part2 input = 
    let calculate (boxNo : int) (lenses : Lens list) = 
        lenses 
        |> List.map snd 
        |> List.mapi (fun i focal -> (boxNo + 1) * (i + 1) * focal)
        |> List.sum
    let ops = input |> parseAllOps 
    let hashmap = Array.replicate 256 [] 
    execute ops hashmap
    hashmap
    |> Array.mapi calculate
    |> Array.sum 

let run fileName =
    let input = readText fileName
    input |> part1 |> printfn "%d"
    input |> part2 |> printfn "%d"
    
"input" |> run
