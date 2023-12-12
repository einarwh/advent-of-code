// Advent of Code 2019. Day 3: Crossed Wires
// dotnet fsi aoc03.fsx

open System
open System.IO

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let parseLine (line : string) = 
    line.Split(",") |> Array.toList |> List.map (fun s -> s[0], s.Substring(1) |> int)

let toOffset (dir, dist) = 
    match dir with
    | 'R' -> (dist, 0)
    | 'L' -> (-dist, 0)
    | 'U' -> (0, dist) 
    | 'D' -> (0, -dist)
    | _ -> failwith <| sprintf "unmatched %c" dir 

let standardizeLineSegment ((x1, y1), (x2, y2)) = 
    if (x1, y1) < (x2, y2) then 
        (x1, y1), (x2, y2)
    else 
        (x2, y2), (x1, y1)

let toPositions offsets = 
    let rec loop (x0, y0) offsets = 
        match offsets with 
        | [] -> []
        | (x, y) :: t -> 
            let pos = (x + x0, y + y0)
            pos :: loop pos t 
    loop (0, 0) offsets

let toLineSegments (offsets : (int * int) list) = 
    offsets 
    |> toPositions 
    |> List.rev
    |> List.pairwise
    |> List.map standardizeLineSegment

let tryFindIntersection ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2)) = 
    if (bx1 = bx2) && (ay1 = ay2) && (ax1 <= bx1 && bx1 <= ax2) && (by1 <= ay1 && ay1 <= by2) then 
        Some (bx1, ay1)
    elif (ax1 = ax2) && (by1 = by2) && (bx1 <= ax1 && ax1 <= bx2) && (ay1 <= by1 && by1 <= ay2) then 
        Some (ax1, by1)
    else 
        None

let findForSegment path segment = 
    path |> List.choose (tryFindIntersection segment)

let findIntersectionPoints lst =
    match lst with
    | [path1; path2] -> 
        path1 |> List.collect (findForSegment path2)
    | _ -> failwith "oof"

let run fileName =
    let lines = readLines fileName |> Array.toList 
    lines 
    |> List.map (parseLine >> List.map toOffset >> toLineSegments) 
    |> findIntersectionPoints
    |> List.map (fun (a, b) -> abs a + abs b)
    |> List.sort 
    |> List.head 
    |> printfn "%d"

"input" |> run
