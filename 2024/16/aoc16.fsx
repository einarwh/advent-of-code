// Advent of Code 2024. Day 16: Reindeer Maze.
// dotnet fsi aoc16.fsx

open System
open System.IO
open System.Collections.Generic

module Maze =
    let get (a : 'a[,]) (x, y) = 
        Array2D.get a y x
    let positions (a : 'a[,]) = 
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]

type Dir = N | W | S | E

type Pos = (int * int)

type Maze = char[,]

type Path = { pos : Pos; dir : Dir }

type Distance = int

type PQ = PriorityQueue<Path * Distance * Path list, Distance>

let move dir (x, y) = 
    match dir with 
    | N -> (x, y - 1)
    | W -> (x - 1, y)
    | S -> (x, y + 1)
    | E -> (x + 1, y)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let turnLeft dir = 
    match dir with 
    | N -> W 
    | W -> S 
    | S -> E 
    | E -> N 

let turnRight dir = 
    match dir with 
    | N -> E 
    | E -> S 
    | S -> W 
    | W -> N

let getPathAhead (x, y) dir = 
    match dir with 
    | N -> { pos = (x, y - 1); dir = N }
    | W -> { pos = (x - 1, y); dir = W } 
    | S -> { pos = (x, y + 1); dir = S }
    | E -> { pos = (x + 1, y); dir = E }

let getPathLeft (x, y) dir = 
    dir |> turnLeft |> getPathAhead (x, y)

let getPathRight (x, y) dir = 
    dir |> turnRight |> getPathAhead (x, y)

let isPathFree maze path = 
    '#' <> Maze.get maze path.pos

let solve startPos maze  = 
    let rec loop (results, visited, q : PQ) = 
        if q.Count = 0 then results 
        else 
            let (path, distance, paths) = q.Dequeue()
            let nextPaths = path :: paths
            let nextVisited = visited |> Set.add path
            let ch = Maze.get maze path.pos
            match ch with 
            | 'E' -> 
                let positions = nextPaths |> List.map (fun p -> p.pos)
                let r = (distance, positions)
                loop ((r :: results), visited, q)
            | 'S' 
            | '.' ->
                let maybeEnqueue p cost = 
                    if not (Set.contains p visited) && isPathFree maze p then 
                        q.Enqueue((p, distance + cost, nextPaths), distance + cost)
                let pathAhead = getPathAhead path.pos path.dir 
                maybeEnqueue pathAhead 1
                let pathLeft = getPathLeft path.pos path.dir 
                maybeEnqueue pathLeft 1001
                let pathRight = getPathRight path.pos path.dir 
                maybeEnqueue pathRight 1001
                loop (results, nextVisited, q)
            | _ -> 
                failwith (sprintf "%c" ch)
    let queue = PQ()
    let startPath = { pos = startPos; dir = E }
    queue.Enqueue((startPath, 0, []), 0)
    let results = loop ([], Set.empty, queue)
    results 
    |> List.groupBy (fst) 
    |> List.map (fun (d, lst) -> (d, lst |> List.collect snd |> Set.ofList |> Set.count))
    |> List.sort
    |> List.head 

let findStartPos maze = 
    let rec find positions = 
        match positions with 
        | [] -> failwith "?"
        | pos :: rest -> 
            if (Maze.get maze pos) = 'S' then pos
            else find rest 
    find <| Maze.positions maze

let run fileName =
    let lines = readLines fileName |> Array.map (Seq.toArray)
    let maze = lines |> array2D
    let startPos = findStartPos maze 
    solve startPos maze |> printfn "%A"

"input" |> run