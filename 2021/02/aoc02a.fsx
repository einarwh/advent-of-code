// Advent of Code 2021. Day 2, part A.
// dotnet fsi aoc02a.fsx

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

let apply (h, d) (cmd : Command) = 
    match cmd with 
    | Forward steps -> (h + steps, d)
    | Down steps -> (h, d + steps)
    | Up steps -> (h, d - steps)

"input.txt"
|> File.ReadAllLines 
|> Array.choose parse
|> Array.fold apply (0, 0)
|> fun (h, d) -> h * d
|> printfn "%d"
