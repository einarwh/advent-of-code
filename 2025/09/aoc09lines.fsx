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

// let toLine ((x1, y1), (x2, y2)) = 
//     let xMin, xMax = if x1 < x2 then (x1, x2) else (x2, x1)
//     let yMin, yMax = if y1 < y2 then (y1, y2) else (y2, y1)
//     (xMin, yMin)
    
//     if x1 = x2 then 
//         let (yStart, yEnd) = if y1 < y2 then (y1, y2) else (y2, y1)
//         [ yStart .. yEnd ] |> List.map (fun y -> (x1, y))
//     else 
//         let (xStart, xEnd) = if x1 < x2 then (x1, x2) else (x2, x1)
//         [ xStart .. xEnd ] |> List.map (fun x -> (x, y1))

let expand ((x1, y1), (x2, y2)) = 
    if x1 = x2 then 
        let (yStart, yEnd) = if y1 < y2 then (y1, y2) else (y2, y1)
        [ yStart .. yEnd ] |> List.map (fun y -> (x1, y))
    else 
        let (xStart, xEnd) = if x1 < x2 then (x1, x2) else (x2, x1)
        [ xStart .. xEnd ] |> List.map (fun x -> (x, y1))

let violates tiles lines boundary (area, ((x1, y1), (x2, y2))) = 
    let xMin, xMax = if x1 < x2 then x1, x2 else x2, x1 
    let xRange = (xMin, xMax)
    let yMin, yMax = if y1 < y2 then y1, y2 else y2, y1 
    let yRange = (yMin, yMax)
    let inRange v (vMin, vMax) = vMin < v && v < vMax 
    let check (x, y) = inRange x xRange && inRange y yRange
    let checkLine ((xLine1, yLine1), (xLine2, yLine2)) = 
        if xLine1 = xLine2 && inRange xLine1 xRange then 
            let yLineMin, yLineMax = if yLine1 < yLine2 then yLine1, yLine2 else yLine2, yLine1 
            inRange xLine1 xRange && yLineMin < yMin && yLineMax > yMax
        else 
            let xLineMin, xLineMax = if xLine1 < xLine2 then xLine1, xLine2 else xLine2, xLine1 
            inRange yLine1 yRange && xLineMin < xMin && xLineMax > xMax
    tiles |> List.exists check || lines |> List.exists checkLine 

let run fileName = 
    let reds = fileName |> readStrings |> List.map parse 
    let rectangles = getRectangles reds 
    rectangles |> List.head |> fst |> printfn "%d"
    let lines = reds |> connect 
    let boundary = lines |> List.collect expand |> Set.ofList 
    rectangles |> List.toSeq |> Seq.filter (fun r -> violates reds lines boundary r |> not) |> Seq.head |> fst |> printfn "%d"

run "input.txt"
