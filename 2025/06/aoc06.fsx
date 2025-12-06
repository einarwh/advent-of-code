// Advent of Code 2025. Day 06: Trash Compactor.
// dotnet fsi aoc06.fsx

open System
open System.IO

let isNonEmpty (cs : char array) = 
    let s = (new string(cs)).Trim()
    s.Length > 0

let rec group (cols : char array array) = 
    if Array.isEmpty cols then []
    else 
        let g = cols |> Array.takeWhile isNonEmpty 
        if g = cols then [ g ]
        else 
            let rest = cols |> Array.skip (1 + g.Length)
            g :: group rest 

let operation (cs : char array) = 
    match (new string(cs)).Trim() with 
    | "*" -> (*)
    | "+" -> (+)
    | _ -> failwith "?"

let human (grid : char array array) = 
    [|0 .. grid.Length - 1|]
    |> Array.map (fun c -> grid |> Array.map (fun r -> r[c]) |> fun cs -> new string(cs) |> int64)

let cephalopod (grid : char array array) = 
    grid |> Array.map (fun cs -> new string(cs) |> int64)

let eval (toNumbers : char array array -> int64 array) (group : char array array) = 
    let op = group |> Array.map Array.last |> operation
    let grid = group |> Array.map (fun cs -> cs[0 .. cs.Length - 2])
    grid |> toNumbers |> Array.reduce op

let run fileName = 
    let rows = fileName |> File.ReadAllLines |> Array.filter (fun line -> line <> String.Empty)
    let cols = [|0 .. rows[0].Length - 1|] |> Array.map (fun x -> rows |> Array.map (fun r -> r[x]))
    let grouped = group cols
    grouped |> List.sumBy (eval human) |> printfn "%d"
    grouped |> List.sumBy (eval cephalopod) |> printfn "%d"

run "input.txt"
