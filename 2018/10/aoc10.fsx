// Advent of Code 2018. Day 10: The Stars Align.
// dotnet fsi aoc10.fsx

open System
open System.IO

type Point = {
    position : int*int
    velocity : int*int
}

let manhattan (x1, y1) (x2, y2) = 
    abs (x1 - x2) + abs (y1 - y2)

let move (p : Point) =
    let (x, y) = p.position 
    let (vx, vy) = p.velocity 
    { position = (x + vx, y + vy); velocity = (vx, vy) }

let nearest (points : Point list) (point : Point) = 
    points |> List.map (fun p -> manhattan point.position p.position) |> List.sort |> List.tail |> List.head 

let findHeight points =
    let positions = points |> List.map (fun p -> p.position)
    let xs = positions |> List.map fst
    let xMin = xs |> List.min 
    let xMax = xs |> List.max 
    xMax - xMin

let findMsg (points : Point list) = 
    let rec loop time lastHeight lastPoints (points : Point list) = 
        let height = findHeight points 
        if height > lastHeight then 
            (time - 1, lastPoints) 
        else 
            let nextPoints = points |> List.map move 
            loop (time + 1) height points nextPoints 
    loop 0 Int32.MaxValue points points

let parse (s : string) = 
    let parts = s.Split([|'<';',';'>'|]) 
    { position = int parts[1], int parts[2]
      velocity = int parts[4], int parts[5] }

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let offset xMin yMin (x, y) = (x - xMin, y - yMin)

let visualize xMin xMax yMin yMax positions = 
    let w = xMax - xMin + 1
    let h = yMax - yMin + 1
    let posSet = positions |> Set.ofList 
    let createRow y = 
        [ 0 .. (w - 1) ] 
        |> List.map (fun x -> if Set.contains (x, y) posSet then "#" else ".")
        |> String.concat ""
    [ 0 .. (h - 1) ] 
    |> List.map createRow |> String.concat "\n" |> printfn "%s"

let run fileName = 
    let lines = readLines fileName
    let points = lines |> List.map parse
    let time, msgPoints = points |> findMsg 
    let positions = msgPoints |> List.map (fun p -> p.position)
    let xs = positions |> List.map fst
    let xMin = xs |> List.min 
    let xMax = xs |> List.max 
    let ys = positions |> List.map snd
    let yMin = ys |> List.min 
    let yMax = ys |> List.max 
    let offsetPositions = positions |> List.map (offset xMin yMin)
    offsetPositions |> visualize xMin xMax yMin yMax 
    printfn ""
    printfn "%d seconds" time

run "input.txt"
