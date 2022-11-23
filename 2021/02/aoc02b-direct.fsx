// Advent of Code 2021. Day 2, part B.
// dotnet fsi aoc02b-direct.fsx

open System
open System.IO
open System.Text.RegularExpressions

let evaluate (h, d, a) line = 
    let pattern = "^([a-z]+) (\d+)$"
    let m = Regex.Match(line, pattern)
    if m.Success then
        let command = m.Groups.[1].Value
        let x = int m.Groups.[2].Value
        match command with
        | "forward" -> (h + x, d + a * x, a)
        | "down" -> (h, d, a + x)
        | "up" -> (h, d, a - x)
        | _ -> (h, d, a)
    else 
        (h, d, a)

"input"
|> File.ReadAllLines 
|> Array.fold evaluate (0, 0, 0)
|> fun (h, d, _) -> h * d
|> printfn "%d"
