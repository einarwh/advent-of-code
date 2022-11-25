// Advent of Code 2021. Day 7. 
// Brute force approach.
// dotnet fsi aoc07.fsx

open System.IO

let parseNumbers (text : string) = 
    text.Trim().Split(",") |> Array.map int 

let uniform steps = steps

let triangular steps = steps * (steps + 1) / 2

let distances (cost : int -> int) (positions : int array) (target : int) = 
    positions |> Array.map (fun pos -> pos - target |> abs |> cost) |> Array.sum

let optimal (cost : int -> int) (positions : int array) = 
    let min = Array.min positions 
    let max = Array.max positions 
    [min .. max] |> List.map (distances cost positions) |> List.min

let run (positions : int array) =
    positions |> optimal uniform |> printfn "Part A (uniform cost): %d" 
    positions |> optimal triangular |> printfn "Part B (triangular cost): %d" 

"input"
|> File.ReadAllText 
|> parseNumbers 
|> Array.sort 
|> run 
