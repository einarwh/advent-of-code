// Advent of Code 2016. Day 18: Like a Rogue.
// TODO: Find loop?
// dotnet fsi aoc18.fsx

open System
open System.IO

let elements = [
    [|'.';'.';'.'|], '.'
    [|'.';'.';'^'|], '^'
    [|'.';'^';'.'|], '.'
    [|'.';'^';'^'|], '^'
    [|'^';'.';'.'|], '^'
    [|'^';'.';'^'|], '.'
    [|'^';'^';'.'|], '^'
    [|'^';'^';'^'|], '.'
]

let lookup = Map(elements)

let getNextRow (row : string) =
    let exp = "." + row + "."
    exp |> Seq.windowed 3 |> Seq.map (fun cs -> Map.find cs lookup) |> Seq.toArray |> fun cs -> new string(cs)

let countSafeTiles (rowCount : int) (row : string) = 
    let rec fn safe count row = 
        let safe' = safe + (row |> Seq.sumBy (fun ch -> if ch = '^' then 0 else 1))
        if count < rowCount then 
            let nxt = getNextRow row 
            fn safe' (count + 1) nxt
        else 
            safe'
    fn 0 1 row 

let run rowCount fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> countSafeTiles rowCount |> printfn "%d"

run 40 "input.txt"
run 400000 "input.txt"
