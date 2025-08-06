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
    let getColumn rows i = rows |> List.map (Seq.item i)
    let columnCount = rows |> List.head |> String.length
    [0 .. columnCount - 1]
    |> List.map (getColumn rows)
    |> List.mapi (fun i col -> if Seq.forall ((=) '.') col then Some (int64 i) else None)
    |> List.choose id 

let findGalaxies rows =
    let rowCount = rows |> List.length 
    let colCount = rows |> List.head |> String.length 
    [ for y in 0 .. rowCount - 1 do 
        for x in 0 .. colCount - 1 do 
            if rows[y][x] = '#' then yield (int64 x, int64 y) ]

let rec findPairs galaxies = 
    match galaxies with 
    | [] -> []
    | h :: t -> 
        let pairs = t |> List.map (fun g -> (h, g)) 
        pairs @ findPairs t 

let findDistance expansion emptyRows emptyColumns ((x1, y1), (x2, y2)) = 
    let xStart = min x1 x2 
    let xStop = max x1 x2
    let yStart = min y1 y2 
    let yStop = max y1 y2 
    let isBetween start stop n = start <= n && n <= stop 
    let countExpansions start stop = 
        List.filter (isBetween start stop) >> List.length >> int64
    let getExpansion start stop indexes =
        (expansion - 1L) * (countExpansions start stop indexes)
    let xExpansion = emptyColumns |> getExpansion xStart xStop 
    let yExpansion = emptyRows |> getExpansion yStart yStop 
    xStop - xStart + xExpansion + yStop - yStart + yExpansion
    
let run expansion fileName = 
    let rows = readLines fileName |> Array.toList
    let emptyRows = findEmptyRows rows 
    let emptyColumns = findEmptyColumns rows 
    rows 
    |> findGalaxies 
    |> findPairs 
    |> List.sumBy (findDistance expansion emptyRows emptyColumns)
    |> printfn "%d"

run 2 "input.txt"
run 1000000 "input.txt"
