// Advent of Code 2024. Day 06
// dotnet fsi aoc06.fsx

open System
open System.IO

module Array2D = 
    let tryGet (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        if first && second then Some (Array2D.get a y x) else None

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
    let rec walk (visited : Set<int*int>) (dir : int*int) (pos : int*int) = 
        let nextPos = move dir pos
        let inFront = nextPos |> Array2D.tryGet board 
        match inFront with 
        | None -> 1 + (visited |> Set.count)
        | Some '#' -> walk visited (turnRight dir) pos 
        | _ -> walk (Set.add pos visited) dir nextPos
    walk Set.empty (0, -1) startPos

let run fileName = 
    let lines = readLines fileName
    let width = lines |> List.head |> Seq.length 
    let height = lines |> List.length 
    let board = Array2D.init height width (fun y x -> lines.[y].[x])
    let startPos = findStartPos board
    board |> patrol startPos |> printfn "%A"

run "input"
