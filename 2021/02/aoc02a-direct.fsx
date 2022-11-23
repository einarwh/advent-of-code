// Advent of Code 2021. Day 2, part A.
// dotnet fsi aoc02a-direct.fsx

open System
open System.IO
open System.Text.RegularExpressions

let evaluate (h, d) line = 
    let pattern = "^([a-z]+) (\d+)$"
    let m = Regex.Match(line, pattern)
    if m.Success then
        let command = m.Groups.[1].Value
        let steps = int m.Groups.[2].Value
        match command with
        | "forward" -> (h + steps, d)
        | "down" -> (h, d + steps)
        | "up" -> (h, d - steps)
        | _ -> (h, d)
    else 
        (h, d)

"input"
|> File.ReadAllLines 
|> Array.fold evaluate (0, 0)
|> fun (h, d) -> h * d
|> printfn "%d"
