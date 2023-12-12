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

let toLineSegments (offsets : (int * int) list) = 
    let folder ((x1, y1), lst) (x2, y2) = 
        let pos = (x1 + x2, y1 + y2)
        (pos, (x1, y1) :: lst)
    offsets 
    |> List.fold folder ((0, 0), []) 
    |> snd 
    |> List.rev
    |> List.pairwise

let tryFindIntersection ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2)) = 
    if ax1 = ax2 then // Vertical 
        if (bx1 <= ax1 && ax1 <= bx2) && (ay1 <= )


let findForSegment path (x1, y1), (x2, y2) = 


let findIntersectionPoints lst = 
    match lst with
    | [path1; path2] -> 
        printfn "find intersection points %A %A" path1 path2
        []
    | _ -> failwith "oof"

let run fileName =
    let lines = readLines fileName |> Array.toList 
    lines 
    |> List.map (parseLine >> List.map toOffset >> toLineSegments) 
    |> findIntersectionPoints
    |> printfn "%A"

"sample1" |> run
