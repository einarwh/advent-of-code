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

let countSymmetries (pattern : string) = 
    let rows = pattern.Split("\n") |> Array.toList
    let cols = rows |> toColumns
    let horizontal = rows |> findSymmetry |> List.sum |> (*) 100
    let vertical = cols |> findSymmetry |> List.sum 
    horizontal + vertical 

let rec toggleChar (chars : char list) index = 
    match chars with
    | [] -> []
    | old :: t -> 
        let ch = 
            if index = 0 then 
                match old with 
                | '#' -> '.'
                | '.' -> '#'
                | _ -> failwith "oof"
            else old 
        ch :: toggleChar t (index - 1)
        
let createLineVariations (line : string) = 
    let chars = Seq.toList line 
    [0 .. String.length line - 1] 
    |> List.map (toggleChar chars >> List.toArray >> String)

let rec toVariations lines : string list list = 
    match lines with 
    | [] -> []
    | line :: rest -> 
        let vars = createLineVariations line 
        let these = vars |> List.map (fun v -> v :: rest)
        let more = rest |> toVariations |> List.map (fun v -> line :: v) 
        these @ more 

let tryFindAlternativeSymmetry (lines : string list) = 
    let originalSymmetries = lines |> findSymmetry 
    let alternativeSymmetries = 
        lines 
        |> toVariations 
        |> List.collect (findSymmetry) 
        |> List.except originalSymmetries
    match alternativeSymmetries with 
    | h :: _ -> Some h 
    | _ -> None 

let countSymmetriesWithSmudge (pattern : string) = 
    let rows = pattern.Split("\n") |> Array.toList
    match tryFindAlternativeSymmetry rows with 
    | Some rowIndex -> 100 * rowIndex 
    | None -> 
        let cols = toColumns rows 
        match tryFindAlternativeSymmetry cols with 
        | Some colIndex -> colIndex
        | None -> 0 

let run fileName =
    let chunks = readChunks fileName
    chunks |> List.map countSymmetries |> List.sum |> printfn "%d"
    chunks |> List.map countSymmetriesWithSmudge |> List.sum |> printfn "%d"

"input" |> run
