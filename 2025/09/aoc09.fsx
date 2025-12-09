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

let expand ((x1, y1), (x2, y2)) = 
    if x1 = x2 then 
        let (yStart, yEnd) = if y1 < y2 then (y1, y2) else (y2, y1)
        [ yStart .. yEnd ] |> List.map (fun y -> (x1, y))
    else 
        let (xStart, xEnd) = if x1 < x2 then (x1, x2) else (x2, x1)
        [ xStart .. xEnd ] |> List.map (fun x -> (x, y1))

let violates i tiles boundary (area, ((x1, y1), (x2, y2))) = 
    // printfn "check box %A" (area, ((x1, y1), (x2, y2)))
    let xRange = if x1 < x2 then x1, x2 else x2, x1 
    let yRange = if y1 < y2 then y1, y2 else y2, y1 
    let inRange v (vMin, vMax) = vMin < v && v < vMax 
    let check (x, y) = 
        inRange x xRange && inRange y yRange
    let checkTile (x, y) = 
        let result = check (x, y)
        // if result then printfn "contains tile %A" (x, y)
        result 
    let checkBoundaryPoint (x, y) = 
        let result = check (x, y)
        // if result then printfn "contains boundary point %A" (x, y)
        result 
    let containsTile = tiles |> List.exists (fun (x, y) -> checkTile (x, y))
    if containsTile then true 
    else 
        let containsBoundaryPoint = boundary |> Set.exists (fun (x, y) -> checkBoundaryPoint (x, y))
        containsBoundaryPoint

let run fileName = 
    let reds = fileName |> readStrings |> List.map parse 
    let rectangles = getRectangles reds 
    rectangles |> List.head |> fst |> printfn "%d"
    let pairs = reds |> connect 
    let boundary = pairs |> List.collect expand |> Set.ofList 
    let result = rectangles |> List.mapi (fun i r -> (i, r)) |> List.toSeq |> Seq.filter (fun (i, r) -> violates i reds boundary r |> not) |> Seq.head 
    printfn "%d" (result |> snd |> fst)

run "input.txt"
