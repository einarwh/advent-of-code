// Advent of Code 2023. Day 5: If You Give A Seed A Fertilizer
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
            let ix = n - src 
            if ix >= 0 && ix < range then 
                dst + ix 
            else 
                next n 
    | _ -> failwith "Wrong"
    
let parseMap (s : string) = 
    let lines = s.Split("\n") |> Array.toList 
    match lines with 
    | [] -> failwith "Nothing"
    | _ :: mappings -> 
        let functions = mappings |> List.map parseMappingLine
        List.foldBack (fun fn next -> fn next) functions id 

let rec seedRanges input = 
    match input with 
    | [] -> []
    | init::range::rest ->
        (init, range) :: seedRanges rest 
    | _ -> failwith "Wrong"

let rangeToSeq (start, count) = 
    seq { for i in start .. (start + count - 1L) -> i }

let readChunks line = 
    let text = File.ReadAllText line 
    text.TrimEnd().Split("\n\n") 
    |> Array.toList 

let run fileName = 
    let chunks = readChunks fileName
    match chunks with 
    | [] -> failwith "Nothing"
    | seedChunk :: rest -> 
        let seeds = seedChunk |> parseNumbers
        let fn = rest |> List.map parseMap |> List.reduce (>>)
        seeds |> List.map fn |> List.min |> printfn "%d"
        seeds
        |> seedRanges 
        |> List.map rangeToSeq
        |> List.map (Seq.map fn >> Seq.min)
        |> List.min
        |> printfn "%d"
    
"input" |> run 
