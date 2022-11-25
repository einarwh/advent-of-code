// Advent of Code 2021. Day 5, part B.
// dotnet fsi aoc05b.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Point = {
    x : int 
    y : int
}

type LineSegment = {
    start : Point
    stop : Point
}

let toPointList (line : LineSegment) : Point list = 
    match line with 
    | { start = { x = x1; y = y1 }
        stop = { x = x2; y = y2 } } ->
        let xStep = if x1 < x2 then 1 else -1 
        let yStep = if y1 < y2 then 1 else -1 
        if x1 = x2 then 
            // Vertical
            [y1 .. yStep .. y2] |> List.map (fun y -> { x = x1; y = y })
        else if y1 = y2 then 
            // Horizontal 
            [x1 .. xStep .. x2] |> List.map (fun x -> { x = x; y = y1 })
        else 
            // Diagonal
            List.zip [x1 .. xStep .. x2] [y1 .. yStep .. y2]
            |> List.map (fun (x, y) -> { x = x; y = y })

let includeSegment (line : LineSegment) : bool = 
    match line with 
    | { start = { x = x1; y = y1 }
        stop = { x = x2; y = y2 } } ->
            x1 = x2 || y1 = y2 || abs (x1 - x2) = abs (y1 - y2)

let parseLineSegment (s : string) : LineSegment option = 
    let pattern = "^(\d+),(\d+) -> (\d+),(\d+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let select (index : int) = int m.Groups.[index].Value
        let x1 = select 1
        let y1 = select 2
        let x2 = select 3
        let y2 = select 4
        Some { 
            start = {x = x1; y = y1}
            stop = {x = x2; y = y2}
        }
    else 
        None

"input"
|> File.ReadAllLines 
|> Array.toList 
|> List.choose parseLineSegment
|> List.filter includeSegment
|> List.collect toPointList
|> List.countBy id
|> List.filter (fun (_, n) -> n >= 2)
|> List.length
|> printfn "Overlapping points: %d"
