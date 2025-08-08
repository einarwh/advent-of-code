// Advent of Code 2022. 
// Day 4: Camp Cleanup.
// dotnet fsi aoc04.fsx

open System.IO
open System.Text.RegularExpressions

let parsePair (line : string) = 
    let pattern = "^(\d+)-(\d+),(\d+)-(\d+)$"
    let m = Regex.Match(line, pattern)
    if m.Success then
        let read (ix : int) = m.Groups.[ix].Value |> int
        Some ((read 1, read 2), (read 3, read 4))
    else 
        None

let contains (a,b) (c,d) = 
    c <= a && d >= b

let overlaps (a,b) (c,d) = 
    a < d && b >= c 

let run pairs = 
    pairs
    |> Array.filter (fun (p1, p2) -> contains p1 p2 || contains p2 p1)
    |> Array.length
    |> printfn "%d"
    pairs
    |> Array.filter (fun (p1, p2) -> overlaps p1 p2 || overlaps p2 p1)
    |> Array.length
    |> printfn "%d"

"input.txt"
|> File.ReadAllLines 
|> Array.choose parsePair
|> run 
