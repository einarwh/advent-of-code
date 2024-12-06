// Advent of Code 2024. Day 06: Guard Gallivant.
// dotnet fsi aoc06.fsx

open System
open System.IO

module Array2D = 
    let tryGet (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        if first && second then Some (Array2D.get a y x) else None
    let fromList (lst : 'a list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let findStartPos (board : char[,]) = 
    let xlen = board.GetLength(1)
    let rec loop y x = 
        match Array2D.get board y x with 
        | '^' -> (x, y)
        | _ -> 
            let (x', y') = 
                if x + 1 = xlen then (0, y + 1) else (x + 1, y)
            loop y' x' 
    loop 0 0 

let move (xStep, yStep) (x, y) = 
    (x + xStep, y + yStep)

let turnRight dir = 
    match dir with 
    | (0, -1) -> (1, 0) // N -> E
    | (1, 0) -> (0, 1) // E -> S
    | (0, 1) -> (-1, 0) // S -> W 
    | (-1, 0) -> (0, -1) // W -> N 
    | _ -> failwith "no direction"

let patrol startPos board = 
    let rec walk visited dir pos = 
        let nextPos = move dir pos
        match nextPos |> Array2D.tryGet board with 
        | None -> visited |> Set.add pos
        | Some '#' -> walk visited (turnRight dir) pos 
        | _ -> walk (Set.add pos visited) dir nextPos
    walk Set.empty (0, -1) startPos

let generateBoards visited board = 
    let addObstruction board (x, y) = 
        let newBoard = Array2D.copy board 
        Array2D.set newBoard y x '#'
        newBoard 
    visited |> Set.toList |> List.map (addObstruction board)

let hasLoop startPos board = 
    let rec walk visited dir pos = 
        if Set.contains (pos, dir) visited then true 
        else 
            let nextPos = move dir pos
            match nextPos |> Array2D.tryGet board with 
            | None -> false
            | Some '#' -> walk visited (turnRight dir) pos 
            | _ -> 
                walk (Set.add (pos,dir) visited) dir nextPos
    walk Set.empty (0, -1) startPos

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let board = Array2D.fromList lines
    let startPos = findStartPos board
    let visited = board |> patrol startPos
    // Part 1
    visited |> Set.count |> printfn "%d"
    // Part 2
    generateBoards visited board
    |> List.filter (hasLoop startPos) 
    |> List.length 
    |> printfn "%d"

run "input"
