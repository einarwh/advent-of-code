// Advent of Code 2020. Day 2, part B.
// dotnet fsi aoc02b.fsx

open System.IO
open System.Text.RegularExpressions

let check (s : string) : string option =
    let m = Regex.Match(s, "(\d+)\-(\d+) ([a-z]): ([a-z]+)")
    if m.Success then
        let pos1 = int m.Groups.[1].Value
        let pos2 = int m.Groups.[2].Value
        let letter = char m.Groups.[3].Value
        let pwd = m.Groups.[4].Value
        match (pwd.[pos1 - 1] = letter, pwd.[pos2 - 1] = letter) with
        | (true, false)
        | (false, true) -> Some pwd
        | _ -> None
    else
        None

File.ReadAllLines "input.txt"
|> Array.choose check
|> Array.length
|> printfn "%d" 