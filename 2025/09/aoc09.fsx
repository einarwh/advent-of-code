// Advent of Code 2025. Day 09: Movie Theater.
// dotnet fsi aoc09.fsx

open System
open System.IO

let readStrings = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let parse (s : string) = 
    match s.Split "," |> Array.map int64 with 
    | [|a; b|] -> (a, b)
    | _ -> failwith "?"

let area (x1, y1) (x2, y2) = 
    (1L + abs (x2 - x1)) * (1L + abs (y2 - y1))

let getRectangles tiles = 
    let rec loop acc tiles = 
        match tiles with 
        | a :: rest -> 
            let rs = rest |> List.map (fun b -> area a b, (a, b)) 
            loop (rs :: acc) rest 
        | _ -> 
            acc |> List.concat |> List.sortByDescending fst 
    loop [] tiles

let connect tiles = 
    match tiles with 
    | [] -> failwith "?"
    | first :: _ -> 
        let rec loop acc tiles = 
            match tiles with 
            | [] -> failwith "?"
            | [ last ] -> (last, first) :: acc |> List.rev
            | a :: b :: t -> 
                loop ((a, b) :: acc) (b :: t)
        loop [] tiles 

let violates tiles lines ((x1, y1), (x2, y2)) = 
    let order (v1, v2) = if v1 < v2 then v1, v2 else v2, v1
    let xMin, xMax = order (x1, x2) 
    let xRange = (xMin, xMax)
    let yMin, yMax = order (y1, y2)
    let yRange = (yMin, yMax)
    let inRange v (vMin, vMax) = vMin < v && v < vMax 
    let check (x, y) = inRange x xRange && inRange y yRange
    let checkLine ((xLine1, yLine1), (xLine2, yLine2)) = 
        if xLine1 = xLine2 then 
            let yLineMin, yLineMax = order (yLine1, yLine2)
            inRange xLine1 xRange && yLineMin <= yMin && yLineMax >= yMax
        else 
            let xLineMin, xLineMax = order (xLine1, xLine2)
            inRange yLine1 yRange && xLineMin <= xMin && xLineMax >= xMax
    tiles |> List.exists check || lines |> List.exists checkLine 

let run fileName = 
    let reds = fileName |> readStrings |> List.map parse 
    let rectangles = getRectangles reds 
    rectangles |> List.head |> fst |> printfn "%d"
    let lines = reds |> connect 
    printfn "%d lines " (List.length lines)
    rectangles |> List.toSeq |> Seq.filter (snd >> violates reds lines >> not) |> Seq.head |> fst |> printfn "%d"

run "sample.txt"
