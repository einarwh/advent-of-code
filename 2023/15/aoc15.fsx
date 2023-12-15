// Advent of Code 2023. Day 15: Lens Library
// dotnet fsi aoc15.fsx

open System
open System.IO

type Op = 
    | Insert of (string * int)
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

let getHash (s : string) : int = 
    let folder current ch  = 
        ((current + int ch) * 17) % 256
    s |> Seq.toList |> List.fold folder 0

let part1 (input : string) = 
    input.Split(",") |> Array.sumBy getHash

let executeOp (hashmap : Map<int, (string * int) list>) (op : Op) = 
    match op with 
    | Remove label -> 
        let remove (maybe : (string * int) list option) = 
            maybe |> Option.map (fun lenses -> lenses |> List.filter (fun (l, _) -> l <> label))
        let hash = getHash label 
        hashmap |> Map.change hash remove
    | Insert (label, focal) -> 
        let insert maybe = 
            match maybe with 
            | Some lenses ->
                if List.exists (fun (l, _) -> l = label) lenses then 
                    lenses 
                    |> List.map (fun (l, f) -> if l = label then (l, focal) else (l, f))
                    |> Some 
                else 
                    Some (lenses @ [label, focal])
            | None -> Some [label, focal]
        let hash = getHash label 
        hashmap |> Map.change hash insert 

let rec execute (ops : Op list) (hashmap : Map<int, (string * int) list>) : Map<int, (string * int) list> = 
    match ops with 
    | [] -> hashmap 
    | op :: rest -> 
        (executeOp hashmap op) |> execute rest 

let part2 (input : string) = 
    let calculate (boxNo : int, lenses : (string * int) list) = 
        lenses |> List.map snd |> List.mapi (fun i focal -> (boxNo + 1) * (i + 1) * focal)
    let ops = input |> parseAllOps 
    Map.empty 
    |> execute ops 
    |> Map.toList 
    |> List.collect calculate
    |> List.sum 

let run fileName =
    let input = readText fileName
    input |> part1 |> printfn "%d"
    input |> part2 |> printfn "%d"
    "."
    
"input" |> run
