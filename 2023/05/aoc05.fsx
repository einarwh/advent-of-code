// Advent of Code 2023. Day 5: If You Give A Seed A Fertilizer.
// This takes a long while to execute.
// dotnet fsi aoc05.fsx

open System.IO
open System.Text.RegularExpressions

let parseNumbers (s : string) : int64 list =
    Regex.Matches(s, "\d+") 
    |> Seq.map (fun m -> int64 m.Value)
    |> Seq.toList

let parseMappingLine (s : string) = 
    match parseNumbers s with 
    | [dst; src; range] -> 
        fun next n ->
            let i = n - src 
            if i >= 0 && i < range then dst + i else next n 
    | _ -> failwith "Wrong"
    
let parseMap (s : string) = 
    match s.Split("\n") |> Array.toList with 
    | [] -> failwith "Nothing"
    | _ :: mappings -> 
        let functions = mappings |> List.map parseMappingLine
        List.foldBack (fun fn next -> fn next) functions id 

let rec seedSequences input = 
    match input with 
    | [] -> []
    | init::range::rest ->
        let sq = seq { for i in init .. (init + range - 1L) -> i }
        sq :: seedSequences rest 
    | _ -> failwith "Wrong"

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let run fileName = 
    match readChunks fileName with 
    | [] -> failwith "Nothing"
    | seedChunk :: rest -> 
        let seeds = seedChunk |> parseNumbers
        let fn = rest |> List.map parseMap |> List.reduce (>>)
        seeds |> List.map fn |> List.min |> printfn "%d"
        seeds
        |> seedSequences 
        |> List.map (Seq.map fn >> Seq.min)
        |> List.min
        |> printfn "%d"
    
"input.txt" |> run 
