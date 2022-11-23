// Advent of Code 2021. Day 2, part B.
// dotnet fsi aoc02b.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Command = 
  | Forward of int 
  | Down of int 
  | Up of int 

let parse (line : string) : Command option = 
    let pattern = "^([a-z]+) (\d+)$"
    let m = Regex.Match(line, pattern)
    if m.Success then
        let commandStr = m.Groups.[1].Value
        let steps = int m.Groups.[2].Value
        match commandStr with
        | "forward" -> Forward steps |> Some
        | "down" -> Down steps |> Some 
        | "up" -> Up steps |> Some
        | _ -> None
    else 
        None

let apply (horizontal, depth, aim) (cmd : Command) = 
    match cmd with 
    | Forward steps -> (horizontal + steps, depth + aim * steps, aim)
    | Down steps -> (horizontal, depth, aim + steps)
    | Up steps -> (horizontal, depth, aim - steps)

"input"
|> File.ReadAllLines 
|> Array.choose parse
|> Array.fold apply (0, 0, 0)
|> fun (h, d, _) -> h * d
|> printfn "%d"
