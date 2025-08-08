// Advent of Code 2015. Day 01: Not Quite Lisp.
// dotnet fsi aoc01.fsx

open System
open System.IO

let trim (input : string) = input.Trim()

let part1 text = 
    let folder acc ch = if ch = '(' then acc + 1 else acc - 1
    let count = text |> Seq.fold folder 0 
    count |> printfn "%d"

let part2 text = 
    let rec find pos level chars = 
        if level = -1 then pos 
        else 
            match chars with 
            | [] -> -1 
            | ch :: rest -> 
                find (pos + 1) (level + (if ch = '(' then 1 else - 1)) rest 
    text |> Seq.toList |> find 0 0 |> printfn "%d"

let run fileName = 
    let text = File.ReadAllText fileName |> trim 
    text |> part1 
    text |> part2 

run "input.txt"
