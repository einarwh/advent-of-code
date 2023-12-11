// Advent of Code 2023. Day 11: Cosmic Expansion
// dotnet fsi aoc11.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let findEmptyRows rows = 
    rows 
    |> List.mapi (fun i row -> if Seq.forall ((=) '.') row then Some (int64 i) else None)
    |> List.choose id 

let findEmptyColumns rows  =
    let getColumn rows i = 
        rows
        |> List.map (Seq.item i)
    let isEmptyColumn = Seq.forall ((=) '.')
    let columnCount = rows |> List.head |> String.length
    let indexes = [0 .. columnCount - 1]
    indexes 
    |> List.map (getColumn rows)
    |> List.mapi (fun i col -> if isEmptyColumn col then Some (int64 i) else None)
    |> List.choose id 

let findGalaxies (lines : string list) =
    let rowCount = lines |> List.length 
    let colCount = lines |> List.head |> Seq.length 
    [ for y in 0 .. rowCount - 1 do 
        for x in 0 .. colCount - 1 do 
            if lines[y][x] = '#' then yield (int64 x, int64 y) ]

let rec findPairs galaxies = 
    match galaxies with 
    | [] -> []
    | h :: t -> 
        let pairs = t |> List.map (fun g -> (h, g)) 
        pairs @ findPairs t 

let findDistance (expansion : int64) emptyRows emptyColumns ((x1, y1), (x2, y2)) = 
    let xStart = min x1 x2 
    let xStop = max x1 x2
    let yStart = min y1 y2 
    let yStop = max y1 y2 
    let isBetween start stop n = start <= n && n <= stop 
    let xExpansions = emptyColumns |> List.filter (isBetween xStart xStop) |> List.length |> int64
    let yExpansions = emptyRows |> List.filter (isBetween yStart yStop) |> List.length |> int64
    let xExpansion = (expansion - 1L) * xExpansions
    let yExpansion = (expansion - 1L) * yExpansions
    xStop - xStart + xExpansion + yStop - yStart + yExpansion
    
let run fileName = 
    let rows = readLines fileName |> Array.toList
    let emptyRows = findEmptyRows rows 
    let emptyColumns = findEmptyColumns rows 
    rows 
    |> findGalaxies 
    |> findPairs 
    |> List.sumBy (findDistance 1000000 emptyRows emptyColumns)
    |> printfn "%d"

"input" |> run 
