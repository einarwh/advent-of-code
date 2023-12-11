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

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let expanded = lines |> expandColumns |> expandRows
    lines
    |> List.iter (fun s -> printfn "%s" s)
    expanded
    |> List.iter (fun s -> printfn "%s" s)
    printfn ","

"sample" |> run 
