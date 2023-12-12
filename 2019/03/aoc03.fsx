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
    let rec loop (x0, y0) offsets result = 
        match offsets with 
        | [] -> result
        | (x, y) :: t -> 
            let pos = (x + x0, y + y0)
            loop pos t (pos :: result)
    loop (0, 0) offsets [] |> List.rev 

let toLineSegments (offsets : (int * int) list) = 
    offsets 
    |> toPositions 
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

let solve1 intersectionPoints = 
    intersectionPoints
    |> List.map (fun (a, b) -> abs a + abs b)
    |> List.sort 
    |> List.head 
    |> printfn "%d"

let toSingleOffsets offset = 
    match offset with 
    | (dist, 0) when dist < 0 -> List.replicate (-dist) (-1, 0)
    | (dist, 0) -> List.replicate dist (1, 0)
    | (0, dist) when dist < 0 -> List.replicate (-dist) (0, -1)
    | (0, dist) -> List.replicate dist (0, 1)
    | _ -> failwith "oof"

let getSteps path1 path2 point = 
    let indexOf = List.findIndex ((=) point)
    indexOf path1 + indexOf path2 + 2

let solve2 intersectionPoints offsetsList =
    let positionsList = offsetsList |> List.map (List.collect toSingleOffsets >> toPositions)
    match positionsList with 
    | [path1; path2] -> 
        let steps = intersectionPoints |> List.map (getSteps path1 path2) |> List.sort |> List.head
        steps |> printfn "%d"
    | _ -> failwith "oof"

let run fileName =
    let lines = readLines fileName |> Array.toList 
    let offsetsList = lines |> List.map (parseLine >> List.map toOffset)
    let intersectionPoints = offsetsList |> List.map toLineSegments |> findIntersectionPoints
    solve1 intersectionPoints 
    solve2 intersectionPoints offsetsList

"input" |> run
