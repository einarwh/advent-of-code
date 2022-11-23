// Advent of Code 2020. Day 2, part A.
// dotnet fsi aoc02a.fsx

open System.IO
open System.Text.RegularExpressions

let check (s : string) : string option =
    let m = Regex.Match(s, "(\d+)\-(\d+) ([a-z]): ([a-z]+)")
    if m.Success then
        let min = int m.Groups.[1].Value
        let max = int m.Groups.[2].Value
        let letter = char m.Groups.[3].Value
        let pwd = m.Groups.[4].Value
        let counted = pwd |> Seq.filter ((=) letter) |> Seq.length
        if min <= counted && counted <= max then
            Some pwd
        else
            None
    else
        None

File.ReadAllLines "input"
|> Array.choose check
|> Array.length
|> printfn "%d"