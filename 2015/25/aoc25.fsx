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

let rowSequence = 
    Seq.unfold (fun (v : int, inc : int) -> Some (v, (v + inc, inc + 1))) (1, 1)

let getColSequence row = 
    let startValue = rowSequence |> Seq.skip (row - 1) |> Seq.head 
    Seq.unfold (fun (v : int, inc : int) -> Some (v, (v + inc, inc + 1))) (startValue, 1 + row)

let getIndex row col = 
    let startValue = rowSequence |> Seq.skip (row - 1) |> Seq.head 
    let colSequence = Seq.unfold (fun (v : int, inc : int) -> Some (v, (v + inc, inc + 1))) (startValue, 1 + row)
    colSequence |> Seq.skip (col - 1) |> Seq.head 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    // let (row, col) = text |> parse
    let row = 2
    let col = 4
    getIndex row col |> printfn "%A"


run "input.txt"
