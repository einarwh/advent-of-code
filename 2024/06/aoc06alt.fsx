// Advent of Code 2024. Day 06: Guard Gallivant.
// dotnet fsi aoc06.fsx

open System
open System.IO
open System.Diagnostics

module Array2D = 
    let tryGet (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        if first && second then Some (Array2D.get a y x) else None
    let numberOfRows (a : 'a[,]) = 
        a.GetLength(0)
    let numberOfColumns (a : 'a[,]) = 
        a.GetLength(1)
    let rowIndexes (a : 'a[,]) = 
        [|0 .. a.GetLength(0) - 1|]
    let columnIndexes (a : 'a[,]) = 
        [|0 .. a.GetLength(1) - 1|]
    let positions (a : 'a[,]) = 
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]
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
    let obstructions = board |> Array2D.positions |> List.filter (fun (x, y) -> '#' = (Array2D.get board y x)) 
    let obsByRow = 
        board 
        |> Array2D.rowIndexes 
        |> Array.map (fun row -> obstructions |> List.choose (fun (x, y) -> if y = row then Some x else None))
    let obsByCol = 
        board 
        |> Array2D.columnIndexes 
        |> Array.map (fun col -> obstructions |> List.choose (fun (x, y) -> if x = col then Some y else None))
    let tryFindLargestSmaller n obsList = 
        let rec tryFind largest lst = 
            match lst with 
            | [] -> largest 
            | a :: rest -> 
                if a < n then tryFind (Some a) rest 
                else largest
        tryFind None obsList
    let tryFindSmallestLarger (n : int) (obsList : int list) = 
        let rec tryFind lst = 
            match lst with 
            | [] -> None 
            | a :: rest -> 
                if a > n then Some a 
                else tryFind rest  
        tryFind obsList
    let rec walk visited dir (pos : int*int) =
        // printfn ""
        // printfn "At %A facing %A having visited %A" pos dir visited
        let (x, y) = pos  
        if Set.contains (pos, dir) visited then true 
        else 
            // Find next obstruction in the given direction.
            // There might not be one -> escape, and there is no loop.
            match dir with 
            | (0, -1) -> 
                // printfn "Moving N"
                match tryFindLargestSmaller y obsByCol.[x] with 
                | Some yObs -> // Move to (x, yObs+1) 
                    // printfn "Obstacle at %A" (x, yObs)
                    let nextP = (x, yObs + 1)
                    // printfn "next pos: %A" nextP
                    let moves = [yObs + 1 .. y] |> List.map (fun i -> ((x, i), dir)) |> Set.ofList
                    // printfn "moves: %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap"
                        let joined = Set.union moves visited 
                        walk joined (turnRight dir) nextP
                    else 
                        true 
                | None -> false 
            | (-1, 0) -> 
                // printfn "Moving W"
                match tryFindLargestSmaller x obsByRow.[y] with 
                | Some xObs -> // Move to (xObs+1, y)
                    // printfn "Obstacle at %A" (xObs, y)
                    let nextP = (xObs + 1, y)
                    // printfn "next pos: %A" nextP
                    let moves = [xObs + 1 .. x] |> List.map (fun i -> ((i, y), dir)) |> Set.ofList
                    // printfn "moves: %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap"
                        let joined = Set.union moves visited 
                        walk joined (turnRight dir) nextP
                    else 
                        true 
                | None -> false 
            | (0, 1) -> 
                // printfn "Moving S"
                match tryFindSmallestLarger y obsByCol.[x] with 
                | Some yObs -> // Move to (x, yObs-1)
                    // printfn "Obstacle at %A" (x, yObs)
                    let nextP = (x, yObs - 1)
                    // printfn "next pos: %A" nextP
                    let moves = [y .. yObs - 1] |> List.map (fun i -> ((x, i), dir)) |> Set.ofList
                    // printfn "moves: %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap"
                        let joined = Set.union moves visited 
                        walk joined (turnRight dir) nextP
                    else 
                        true 
                | None -> false 
            | (1, 0) -> 
                // printfn "Moving E"
                match tryFindSmallestLarger x obsByRow.[y] with 
                | Some xObs -> // Move to (xObs-1, y)
                    // printfn "Obstacle at %A" (xObs, y)
                    let nextP = (xObs-1, y)
                    // printfn "next pos: %A" nextP
                    let moves = [x .. xObs - 1] |> List.map (fun i -> ((i, y), dir)) |> Set.ofList
                    // printfn "moves: %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap"
                        let joined = Set.union moves visited 
                        walk joined (turnRight dir) nextP
                    else 
                        true 
                | None -> false 
    walk Set.empty (0, -1) startPos

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let board = Array2D.fromList lines
    let startPos = findStartPos board
    let visited = board |> patrol startPos
    // Part 1
    visited |> Set.count |> printfn "%d"
    // Part 2
    let sw = Stopwatch.StartNew()
    generateBoards visited board
    |> List.filter (hasLoop startPos) 
    |> List.length 
    |> printfn "%d"
    printfn "%A" sw.Elapsed

run "input"
