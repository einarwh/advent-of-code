// Advent of Code 2021. Day 7, Part B. 
// Brute force approach.
// dotnet fsi aoc07b.fsx

open System.IO

let parseNumbers (text : string) = 
    text.Trim().Split(",") |> Array.map int 

let triangular steps = 
    steps * (steps + 1) / 2

let distances (numbers : int array) (target : int) = 
    numbers |> Array.map (fun n -> n - target |> abs |> triangular) |> Array.sum

let run (numbers : int array) = 
    let min = Array.min numbers 
    let max = Array.max numbers 
    [min .. max] |> List.map (distances numbers) |> List.min 

"input"
|> File.ReadAllText 
|> parseNumbers 
|> Array.sort 
|> run 
|> printfn "%d"
