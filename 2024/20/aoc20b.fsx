// Advent of Code 2024. Day 20: Race Condition.
// dotnet fsi aoc20.fsx

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics 

module Array2D =
    let inBounds (a : 'a[,]) (x, y) =
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        first && second
    let getPos (a : 'a[,]) (x, y) =
        Array2D.get a y x
    let positions (a : 'a[,]) =
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]

type Dir = N | W | S | E

type Pos = (int * int)

type Racetrack = char[,]

type Path = { pos : Pos; dir : Dir }

type Distance = int

type PQ = PriorityQueue<Pos * Distance * (Pos * Distance) list, Distance>

let getNeighbours (x, y) =
    [ (x, y - 1); (x - 1, y); (x, y + 1); (x + 1, y) ]

let isWall racetrack pos  =
    '#' = Array2D.getPos racetrack pos

let race (startPos : Pos) (racetrack : Racetrack)  =
    let rec loop (visited : Set<Pos>, q : PQ) =
        if q.Count = 0 then []
        else
            let (pos, distance, posDistList) = q.Dequeue()
            let nextPosList = (pos, distance) :: posDistList
            let nextVisited = visited |> Set.add pos
            let ch = Array2D.getPos racetrack pos
            match ch with
            | 'E' ->
                nextPosList
            | 'S'
            | '.' ->
                let next =
                    getNeighbours pos
                    |> List.filter (Array2D.inBounds racetrack)
                    |> List.filter (fun p -> not (Set.contains p visited))
                    |> List.filter (fun p -> not (isWall racetrack p))
                next |> List.iter (fun p -> q.Enqueue((p, distance + 1, nextPosList), distance + 1))
                let nextVisited = next |> Set.ofList |> Set.union visited
                loop (nextVisited, q)
            | _ ->
                failwith (sprintf "%c" ch)
    let queue = PQ()
    queue.Enqueue((startPos, 0, []), 0)
    loop (Set.empty, queue) |> List.rev 

let findStartPos (racetrack : Racetrack) : Pos =
    let rec find positions =
        match positions with
        | [] -> failwith "?"
        | pos :: rest ->
            if (Array2D.getPos racetrack pos) = 'S' then pos
            else find rest
    find <| Array2D.positions racetrack

let getReachablesInDuration (cheatDuration : int) (x, y) : Pos list=
    let chooser (x, y) (dx, dy) = 
        let len = abs dx + abs dy 
        if len > 0 && len <= cheatDuration then Some (x + dx, y + dy) else None 
    let range = [ -cheatDuration .. cheatDuration ]
    range |> List.collect (fun dy -> range |> List.choose (fun dx -> chooser (x, y) (dx, dy)))

let posDiff (x1, y1) (x2, y2) = 
    abs (x2 - x1) + abs (y2 - y1) 

let findCheats cheatDuration (posDistMap : Map<Pos, Distance>) (pos, dist) =
    getReachablesInDuration cheatDuration pos
    |> List.choose (fun (p : Pos) -> Map.tryFind p posDistMap |> Option.map (fun d -> (p, d - dist)))
    |> List.choose (fun (p, diff) -> if diff - (posDiff pos p) > 0 then Some (pos, p, diff - (posDiff pos p)) else None)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let solve cheatDuration posDistList = 
    let stopwatch = Stopwatch.StartNew()
    let posDistMap = Map.ofList posDistList
    let result = posDistList |> List.collect (findCheats cheatDuration posDistMap) |> List.sortBy (fun (p1, p2, saved) -> saved) |> List.rev
    let savings = result |> List.map (fun (_, _, saved) -> saved)
    savings |> List.filter (fun saved -> saved >= 100) |> List.length |> printfn "%d"
    printfn "%A ms" stopwatch.Elapsed.TotalMilliseconds

let run fileName =
    let lines = readLines fileName |> Array.map (Seq.toArray)
    let racetrack = lines |> array2D
    let startPos = findStartPos racetrack
    let posDistList = race startPos racetrack
    posDistList |> solve 2 
    posDistList |> solve 20

run "input"