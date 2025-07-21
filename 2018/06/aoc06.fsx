// Advent of Code 2018. Day 06: Chronal Coordinates.
// dotnet fsi aoc06.fsx

open System
open System.IO

type Box = {
    xMin : int 
    xMax : int
    yMin : int 
    yMax : int
}

let parse (s : string) : (int*int) option = 
    match s.Split(", ") with 
    | [|a; b|] -> Some (int a, int b)
    | _ -> None 

let getRange vals = 
    let rec fn minVal maxVal vals = 
        match vals with 
        | [] -> (minVal, maxVal)
        | v :: rest -> 
            fn (min minVal v) (max maxVal v) rest 
    fn Int32.MaxValue Int32.MinValue vals 

let createBox (points : (int*int) list) : Box = 
    let (xMin, xMax) = points |> List.map fst |> getRange
    let (yMin, yMax) = points |> List.map snd |> getRange
    { xMin = xMin; xMax = xMax; yMin = yMin; yMax = yMax }

let createBoundary (points : (int*int) list) : Set<int*int> = 
    let (xMin, xMax) = points |> List.map fst |> getRange
    let (yMin, yMax) = points |> List.map snd |> getRange
    Set.empty 
    |> Set.union ([xMin .. xMax] |> List.map (fun x -> (x, yMin)) |> Set.ofList) 
    |> Set.union ([xMin .. xMax] |> List.map (fun x -> (x, yMax)) |> Set.ofList) 
    |> Set.union ([yMin .. yMax] |> List.map (fun y -> (xMin, y)) |> Set.ofList) 
    |> Set.union ([yMin .. yMax] |> List.map (fun y -> (xMax, y)) |> Set.ofList) 

let manhattan (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)

let manhattans coordinates pos = 
    coordinates |> List.map (fun c -> manhattan pos c, c) |> List.sort 

let getPointsForCoordinates (box : Box) coordinates = 
    let selectCoordinate distances = 
        match distances |> List.take 2 with 
        | [ (dist1, coord1); (dist2, coord2) ] -> 
            if dist1 = dist2 then None else Some coord1
        | _ -> failwith "?"
    let determined (maybeCoord, lst) = 
        match maybeCoord with 
        | Some coord -> Some (coord, lst |> List.map fst)
        | None -> None
    let positions = [ for x in [box.xMin .. box.xMax] do for y in [box.yMin .. box.yMax] do yield (x, y) ]
    positions 
    |> List.map (fun p -> (p, manhattans coordinates p |> selectCoordinate))
    |> List.groupBy snd 
    |> List.choose determined 

let selectLargest boundary (coordinatesWithPoints : List<(int*int)*List<(int*int)>>) =
    let isInfinite points = 
        points |> List.exists (fun p -> Set.contains p boundary)
    let finite = coordinatesWithPoints |> List.filter (snd >> isInfinite >> not)
    let areas = finite |> List.map (fun (_, lst) -> lst |> List.length) 
    areas |> List.max

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let coordinates = lines |> List.choose parse
    let box = coordinates |> createBox
    let boundary = coordinates |> createBoundary
    coordinates |> getPointsForCoordinates box |> selectLargest boundary |> printfn "%d"
    

run "input.txt"
