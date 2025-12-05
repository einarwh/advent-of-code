// Advent of Code 2025. Day 05: Cafeteria.
// dotnet fsi aoc05.fsx

open System
open System.IO

let parseRanges (s : string) = 
    match s.Split "-" with 
    | [|s1; s2|] -> int64 s1, int64 s2 
    | _ -> failwith "?"

let fresh ingredient (start, stop)  = 
    start <= ingredient && ingredient <= stop 

let run fileName = 
    let text = File.ReadAllText fileName
    let chunk1, chunk2 = 
        match text.Split "\n\n" with 
        | [|s1; s2|] -> s1.Trim(), s2.Trim()
        | _ -> failwith "?"
    let ranges = chunk1.Split "\n" |> Array.map parseRanges 
    let ingredients = chunk2.Split "\n" |> Array.map int64 
    chunk1.Split "\n" |> Array.map parseRanges |>  printfn "%A"
    ingredients 
    |> Array.filter (fun it -> ranges |> Array.exists (fresh it)) 
    |> Array.length
    |> printfn "%d"

run "input.txt"
