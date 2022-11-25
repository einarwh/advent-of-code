// Advent of Code 2021. Day 5, part A.
// dotnet fsi aoc05a.fsx

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
        if x1 = x2 then 
            let (yFirst, yLast) = if y1 < y2 then (y1, y2) else (y2, y1)
            [yFirst .. yLast] |> List.map (fun y -> { x = x1; y = y })
        else 
            let (xFirst, xLast) = if x1 < x2 then (x1, x2) else (x2, x1)
            [xFirst .. xLast] |> List.map (fun x -> { x = x; y = y1 })

let isHorizontalOrVertical (line : LineSegment) : bool = 
    match line with 
    | { start = { x = x1; y = y1 }
        stop = { x = x2; y = y2 } } ->
            x1 = x2 || y1 = y2

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
|> List.filter isHorizontalOrVertical
|> List.collect toPointList
|> List.countBy id
|> List.filter (fun (_, n) -> n >= 2)
|> List.length
|> printfn "Overlapping points: %d"
