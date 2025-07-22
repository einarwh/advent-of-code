// Advent of Code 2015. Day 25: Let It Snow.
// dotnet fsi aoc25.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parse (s : string) : int * int = 
    let m = Regex.Match(s, "row (\d+), column (\d+)")
    if m.Success then
        let row = m.Groups.[1].Value |> int 
        let col = m.Groups.[2].Value |> int 
        (row, col)
    else
        failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> parse |> printfn "%A"

run "input.txt"
