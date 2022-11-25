// Advent of Code 2021. Day 7, Part A.
// dotnet fsi aoc07a.fsx

open System.IO

let parseNumbers (text : string) = 
    text.Trim().Split(",") |> Array.map int 

let distances (target : int) (numbers : int array) = 
    numbers |> Array.map (fun n -> n - target |> abs) |> Array.sum

let optimal (numbers : int array) = 
    let med = numbers.[numbers.Length / 2]
    distances med numbers 
    
"input"
|> File.ReadAllText 
|> parseNumbers 
|> Array.sort 
|> optimal 
|> printfn "%d"
