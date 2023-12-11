// Advent of Code 2023. Day 11: Cosmic Expansion
// dotnet fsi aoc11.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let findEmptyColumns rows  =
    let getColumn rows i = 
        rows
        |> List.map (Seq.item i)
    let isEmptyColumn = Seq.forall ((=) '.')
    let columnCount = rows |> List.head |> String.length
    let indexes = [0 .. columnCount - 1]
    indexes 
    |> List.map (getColumn rows)
    |> List.mapi (fun i col -> if isEmptyColumn col then Some i else None)
    |> List.choose id 

let expandColumns rows = 
    let rec insertAt ix (row : char list) = 
        match row with 
        | [] -> []
        | h :: t -> 
            let rest = if ix = 0 then row else insertAt (ix - 1) t 
            h :: rest    
    let rec expandRow indexes (row : char list) = 
        match indexes with 
        | [] -> row 
        | ix :: rest -> expandRow rest (insertAt ix row)
    let emptyColumns = findEmptyColumns rows 
    let adjusted = emptyColumns |> List.mapi (fun i ix -> i + ix)
    rows 
    |> List.map (Seq.toList >> expandRow adjusted >> List.toArray >> String)

let rec expandRows rows = 
    match rows with 
    | [] -> []
    | row :: rest ->
        if row |> Seq.forall ((=) '.') then 
            row :: row :: expandRows rest 
        else 
            row :: expandRows rest

let findGalaxies (lines : string list) =
    let rowCount = lines |> List.length 
    let colCount = lines |> List.head |> Seq.length 
    [ for y in 0 .. rowCount - 1 do 
        for x in 0 .. colCount - 1 do 
            if lines[y][x] = '#' then yield (x, y) ]

let rec findPairs galaxies = 
    match galaxies with 
    | [] -> []
    | h :: t -> 
        let pairs = t |> List.map (fun g -> (h, g)) 
        pairs @ findPairs t 

let findDistance ((x1, y1), (x2, y2)) = 
    abs (x2 - x1) + abs (y2 - y1)
    
let run fileName = 
    let rows = readLines fileName |> Array.toList
    rows 
    |> expandColumns 
    |> expandRows
    |> findGalaxies 
    |> findPairs 
    |> List.sumBy findDistance 
    |> printfn "%A"

"sample" |> run 
