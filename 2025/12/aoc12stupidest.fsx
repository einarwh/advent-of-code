// Advent of Code 2025. Day 12: Christmas Tree Farm.
// dotnet fsi aoc12stupidest.fsx

open System.IO

let parseRegionSize (s : string) = 
    match s.Split "x" with 
    | [|a; b|] -> int a, int b 
    | _ -> failwith <| sprintf "%s?" s

let parseNumbers (s : string) = 
    s.Split " " |> Array.toList |> List.map int 
    
let parseRegion (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> parseRegionSize a, parseNumbers b
    | _ -> failwith <| sprintf "'%s'?" s

let parseRegions (s : string) = 
    s.Trim().Split "\n" |> Array.toList |> List.map parseRegion
    
let run fileName = 
    let text = File.ReadAllText fileName 
    let chunks = text.Split "\n\n" |> Array.toList |> List.rev 
    let stupidest ((w, h), numbers) = w*h >= 9 * List.sum numbers
    chunks |> List.head |> parseRegions |> List.filter stupidest |> List.length |> printfn "%d"

run "input.txt"
