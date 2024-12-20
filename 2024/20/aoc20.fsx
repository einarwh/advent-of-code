// Advent of Code 2024. Day 20: Race Condition.
// dotnet fsi aoc20.fsx

open System
open System.IO
open System.Collections.Generic

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
    [ (x, y - 1)
      (x - 1, y)
      (x, y + 1)
      (x + 1, y) ]

let move dir (x, y) =
    match dir with
    | N -> (x, y - 1)
    | W -> (x - 1, y)
    | S -> (x, y + 1)
    | E -> (x + 1, y)

let isPosFree racetrack pos  =
    '#' <> Array2D.getPos racetrack pos

let isWall racetrack pos  =
    '#' = Array2D.getPos racetrack pos

let solve (startPos : Pos) (racetrack : Racetrack)  =
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
    let results = loop (Set.empty, queue)
    results |> List.rev
    // results |> printfn "%A"

let findStartPos (racetrack : Racetrack) : Pos =
    let rec find positions =
        match positions with
        | [] -> failwith "?"
        | pos :: rest ->
            if (Array2D.getPos racetrack pos) = 'S' then pos
            else find rest
    find <| Array2D.positions racetrack

let getReachables (x, y) : Pos list=
    [ (x, y-2)
      (x-1, y-1); (x+1, y-1)
      (x-2, y); (x+2, y)
      (x-1, y+1); (x+1, y+1)
      (x, y+2) ]

let findCheatsAt (posDistMap : Map<Pos, Distance>) (pos, dist) =
    // printfn "\nfindCheatsAt position %A (distance %d)" pos dist
    let reachables = getReachables pos
    // printfn "Reachable positions: %A" reachables
    let onRoute = reachables |> List.choose (fun (p : Pos) -> Map.tryFind p posDistMap |> Option.map (fun d -> (p, d)))
    // printfn "Reachables on route %A" onRoute
    // let cheats = reachables |> List.choose (fun (p : Pos) -> Map.tryFind p posDistMap) |> List.map (fun d -> (d - dist)) |> List.filter (fun diff -> diff > 2)
    let cheats1 =
        reachables
        |> List.choose (fun (p : Pos) -> Map.tryFind p posDistMap |> Option.map (fun d -> (p, d - dist)))
        |> List.choose (fun (p, diff) -> if diff - 2 > 0 then Some (pos, p, diff - 2) else None)
    // printfn "Cheats on route %A" cheats1
    cheats1

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName |> Array.map (Seq.toArray)
    // printfn "%A" lines
    let racetrack = lines |> array2D
    let startPos = findStartPos racetrack
    printfn "%A" startPos
    let posDistList = solve startPos racetrack
    // printfn "LIST %A" posDistList
    // printfn "%A" (findReachable (3, 3) |> List.sort)
    let posDistMap = Map.ofList posDistList
    // printfn "MAP %A" posDistMap

    let result = posDistList |> List.collect (findCheatsAt posDistMap) |> List.sortBy (fun (p1, p2, saved) -> saved) |> List.rev
    let savings = result |> List.map (fun (_, _, saved) -> saved)
    let bigSavings = savings |> List.filter (fun saved -> saved >= 100) |> List.length

    printfn "%d" bigSavings



    // lines |> printfn "%A"
    0

run "input"
