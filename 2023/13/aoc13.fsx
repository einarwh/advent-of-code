// Advent of Code 2023. Day 13: Point of Incidence
// dotnet fsi aoc13.fsx

open System
open System.IO

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let rec safeZip lst1 lst2 = 
    match lst1, lst2 with 
    | [], _ -> []
    | _, [] -> []
    | h1::t1, h2::t2 -> (h1, h2) :: safeZip t1 t2

let isSymmetryPoint lines index = 
    let before, after = List.splitAt index lines 
    safeZip (List.rev before) after |> List.forall (fun (a, b) -> a = b)

let findSymmetry lines = 
    lines 
    |> List.pairwise
    |> List.mapi (fun i (a, b) -> if a = b then Some (i + 1) else None)
    |> List.choose id 
    |> List.filter (isSymmetryPoint lines)

let toColumns rows = 
    let len = rows |> List.head |> String.length
    [0 .. len - 1]
    |> List.map (fun i -> rows |> List.map (fun r -> r[i]) |> List.toArray |> String)

let countSymmetries (chunk : string) = 
    let rows = chunk.Split("\n") |> Array.toList
    let cols = rows |> toColumns
    let horizontal = rows |> findSymmetry |> List.sum |> (*) 100
    let vertical = cols |> findSymmetry |> List.sum 
    horizontal + vertical 

let run fileName =
    let chunks = readChunks fileName
    chunks |> List.map countSymmetries |> List.sum |> printfn "%d"

"input" |> run
