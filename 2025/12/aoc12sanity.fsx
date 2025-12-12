// Advent of Code 2025. Day 12: Christmas Tree Farm.
// dotnet fsi aoc12stupidest.fsx

open System.IO

let parseRegionSize (s : string) = 
    match s.Split "x" with 
    | [|a; b|] -> int a, int b 
    | _ -> failwith <| sprintf "%s?" s

let parseNumbers (s : string) = 
    s.Split " " |> Array.toList |> List.map int 
    
let tryParseRegion (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> Some (parseRegionSize a, parseNumbers b)
    | _ -> None 

let run fileName = 
    let stupidest ((w, h), numbers) = (w / 3) * (h / 3) >= List.sum numbers
    let regions = File.ReadAllLines fileName |> Array.toList |> List.choose tryParseRegion
    regions |> List.filter stupidest |> List.length |> printfn "%d"

run "input.txt"
