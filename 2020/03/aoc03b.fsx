// Advent of Code 2020. Day 3, Part B.
// dotnet fsi aoc03b.fsx

open System.IO

let trees (lines : string array) (right : int, left : int) : int64 =
    lines
    |> Array.mapi (fun i line -> (i, line))
    |> Array.filter (fun (i, _) -> i % left = 0)
    |> Array.map (fun (i, line) -> if '#' = line.[(right * i) % line.Length] then 1L else 0L)
    |> Array.sum

let lines = "input.txt" |> File.ReadAllLines
let configs = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
configs
|> List.map (trees lines)
|> List.fold (*) 1L
|> printfn "%d"
