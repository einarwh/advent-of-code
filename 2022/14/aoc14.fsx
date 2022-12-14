// Advent of Code 2022. 
// Day 14: Regolith Reservoir.
// dotnet fsi aoc14.fsx

open System.IO 

type Pos = (int * int)

type Line = (Pos * Pos)

let toPos (s : string) = 
    match s.Split(",") with 
    | [|a;b|] -> (int a, int b)
    | _ -> failwith "not a valid position"

let toPositions (pos1 : Pos, pos2 : Pos) : Pos list = 
    let (startPos, endPos) = if pos1 < pos2 then (pos1, pos2) else (pos2, pos1)
    match (startPos, endPos) with 
    | ((x1, y1), (x2, y2)) -> 
        if x1 = x2 then 
            [y1 .. y2] |> List.map (fun y -> (x1, y))
        else 
            [x1 .. x2] |> List.map (fun x -> (x, y1))

let parseLine (s : string) = 
    s.Split(" -> ")
    |> Array.toList 
    |> List.map toPos
    |> List.pairwise
    |> List.collect toPositions
    |> Set.ofList

"sample"
|> File.ReadAllLines
|> Array.toList 
|> List.map parseLine
|> printfn "%A"