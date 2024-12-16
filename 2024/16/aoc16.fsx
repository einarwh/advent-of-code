// Advent of Code 2024. Day 16: Reindeer Maze.
// dotnet fsi aoc16.fsx

open System
open System.IO
open System.Collections.Generic

module Maze =
    let getRowCount (a : 'a[,]) = a.GetLength(0)
    let getColumnCount (a : 'a[,]) = a.GetLength(1)
    let get (a : 'a[,]) (x, y) = 
        Array2D.get a y x
    let positions (a : 'a[,]) = 
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]

type Dir = N | W | S | E

type Pos = (int*int)

type Maze = char[,]

type Path = {
    pos : Pos 
    dir : Dir 
}

type Distance = int64
// type DistanceMap = Map<Path, int> 

type PQ = PriorityQueue<Path * Distance, Distance>

let move dir (x, y) = 
    match dir with 
    | N -> (x, y - 1)
    | W -> (x - 1, y)
    | S -> (x, y + 1)
    | E -> (x + 1, y)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

// let tryMove dir (path, heat) maze (visited, queue : PQ) =  
//     let yMax = (maze |> Array2D.getRowCount) - 1
//     let xMax = (maze |> Array2D.getColumnCount) - 1
//     let (x, y) = move dir path.pos 
//     if x < 0 || x > xMax || y < 0 || y > yMax then 
//         (visited, queue) 
//     else 
//         let steps = if dir = path.dir then path.steps else 0
//         let next = {
//             pos = (x, y)
//             dir = dir 
//             steps = steps + 1
//         }
//         if visited |> Set.contains next then 
//             (visited, queue) 
//         else
//             let h = heat + Array2D.get maze y x 
//             queue.Enqueue((next, h), h)
//             (visited |> Set.add next, queue)

let findNeighbours maze (x, y) = 
    let possible =
        [ { pos = (x, y - 1); dir = N } 
          { pos = (x - 1, y); dir = W } 
          { pos = (x, y + 1); dir = S } 
          { pos = (x + 1, y); dir = E } ]
    possible |> List.filter (fun path -> Maze.get maze path.pos <> '#')

let oppositeDirection dir = 
    match dir with 
    | N -> S 
    | W -> E 
    | S -> N 
    | E -> W

let getPathInDirection (x, y) dir = 
    match dir with 
    | N -> { pos = (x, y - 1); dir = N }
    | W -> { pos = (x - 1, y); dir = W } 
    | S -> { pos = (x, y + 1); dir = S }
    | E -> { pos = (x + 1, y); dir = E }

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

let isPosBlocked maze (x, y) = 
    '#' = Maze.get maze (x, y)

let isPathBlocked maze path = 
    isPosBlocked maze path.pos 

let isPathFree maze path = not <| isPathBlocked maze path

let findPossibleMoves maze path = 
    let dirs = [ N; W; S; E ] |> List.filter (fun d -> d <> oppositeDirection path.dir)
    dirs 
    |> List.map (getPathInDirection path.pos)
    |> List.filter (fun path -> '#' <> Maze.get maze path.pos)

let solve startPos (maze : Maze) : Distance = 
    let rec loop (visited : Set<Path>, q : PQ) = 
        // printfn "q elements: %d" q.Count 
        if q.Count = 0 then None 
        else 
            let (path, distance) = q.Dequeue()
            // printfn "Dequeued %A" (path, distance)
            let nextVisited = visited |> Set.add path
            // printfn "Visited %A" path
            let ch = Maze.get maze path.pos
            match ch with 
            | 'E' -> 
                // End!
                Some distance 
            | 'S' 
            | '.' ->
                // Free space!
                let maybeEnqueue path cost = 
                    if not (Set.contains path visited) && isPathFree maze path then 
                        q.Enqueue((path, distance + cost), distance + cost)

                // Continue? 
                let pathAhead = getPathAhead path.pos path.dir 
                maybeEnqueue pathAhead 1L 

                // Turn left?
                let pathLeft = getPathLeft path.pos path.dir 
                maybeEnqueue pathLeft 1001L

                // Turn right?
                let pathRight = getPathRight path.pos path.dir 
                maybeEnqueue pathRight 1001L

                // Keep going.
                loop (nextVisited, q)
            | '#' -> 
                failwith "#"
            | _ -> 
                failwith (sprintf "%c" ch)
    let queue = PQ()
    // Find possible directions here. 
    let startPath = { pos = startPos; dir = E }
    queue.Enqueue((startPath, 0), 0)
    match loop (Set.empty, queue) with 
    | Some distance -> distance 
    | None -> failwith "?"

let findStartPos (maze : Maze) : Pos = 
    let rec find positions = 
        match positions with 
        | [] -> failwith "?"
        | pos :: rest -> 
            if (Maze.get maze pos) = 'S' then pos
            else find rest 
    find <| Maze.positions maze

let run fileName =
    let charAsInt ch = Char.GetNumericValue(ch) |> int
    let lines = readLines fileName |> Array.map (Seq.toArray)
    let maze : char[,] = lines |> array2D
    let startPos = findStartPos maze 
    solve startPos maze |> printfn "%A"

"input" |> run