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

type Boundary = Obstacle of int | Outside

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

let traceRoute startPos board = 
    let rec walk visited dir pos = 
        let nextPos = move dir pos
        match nextPos |> Array2D.tryGet board with 
        | None -> (pos :: visited) |> List.rev
        | Some '#' -> walk visited (turnRight dir) pos 
        | _ -> walk (pos :: visited) dir nextPos
    walk [] (0, -1) startPos

let addObstruction board (x, y) = 
    let newBoard = Array2D.copy board 
    Array2D.set newBoard y x '#'
    newBoard 

let generateBoards visited board = 
    visited |> Set.toList |> List.map (addObstruction board)

let hasLoop (intervalsByRow : (Boundary*Boundary) list array) (intervalsByCol : (Boundary*Boundary) list array) startPos board = 
    // printf "."
    // printfn "---------------------"
    // printfn "Check board for loop!"
    let between (n : int) (startBoundary : Boundary, stopBoundary : Boundary) : bool =
        match (startBoundary, stopBoundary) with 
        | Outside, Obstacle upper -> n <= upper 
        | Obstacle lower, Obstacle upper -> lower <= n && n <= upper 
        | Obstacle lower, Outside -> lower <= n 
        | Outside, Outside -> true 
    let tryFindLargest (n : int) (intervals : (Boundary*Boundary) list) : int option = 
        // printfn "tryFindLargest: %d - %A" n intervals
        match intervals |> List.find (between n) with 
        | Outside, Obstacle upper -> Some (upper - 1)  
        | Obstacle lower, Obstacle upper -> Some (upper - 1) 
        | Obstacle lower, Outside -> None  
        | _ -> None 
    let tryFindSmallest (n : int) (intervals : (Boundary*Boundary) list) : int option = 
        // printfn "tryFindSmallest: %d - %A" n intervals
        match intervals |> List.find (between n) with 
        | Outside, Obstacle upper -> None   
        | Obstacle lower, Obstacle upper -> Some (lower + 1) 
        | Obstacle lower, Outside -> Some (lower + 1)  
        | _ -> None 
    let rec walk route visited dir (pos : int*int) =
        // printfn ""
        // printfn "At %A facing %A having visited %A" pos dir visited
        let (x, y) = pos  
        if Set.contains (pos, dir) visited then 
            // printfn "Found loop!"
            true 
        else 
            // Find next obstruction in the given direction.
            // There might not be one -> escape, and there is no loop.
            match dir with 
            | (0, -1) -> 
                // printfn "Moving N"
                match tryFindSmallest y intervalsByCol.[x] with 
                | None -> 
                    // printfn "Found no obstacle - exiting. No loop."
                    false 
                | Some smallest -> 
                    // printfn "Found obstacle at %A" (x, smallest - 1)
                    let nextP = (x, smallest)
                    // printfn "Move to %A" nextP
                    let movesList = [smallest .. y] |> List.map (fun i -> ((x, i), dir)) |> List.rev
                    let moves = movesList |> Set.ofList
                    // printfn "Add moves %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap!"
                        let joined = Set.union moves visited 
                        walk (route @ movesList) joined (turnRight dir) nextP
                    else 
                        // printfn "Overlap found -> there was a loop!"
                        true 
            | (-1, 0) -> 
                // printfn "Moving W"
                match tryFindSmallest x intervalsByRow.[y] with 
                | None ->
                    // printfn "Found no obstacle - exiting. No loop."
                    false
                | Some xSmallest -> 
                    // printfn "Found obstacle at %A" (xSmallest - 1, y)
                    let nextP = (xSmallest, y)
                    // printfn "Move to %A" nextP
                    let movesList = [xSmallest .. x] |> List.map (fun i -> ((i, y), dir)) |> List.rev
                    let moves = movesList |> Set.ofList
                    // printfn "Add moves %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap!"
                        let joined = Set.union moves visited 
                        walk (route @ movesList) joined (turnRight dir) nextP
                    else 
                        // printfn "Overlap found -> there was a loop!"
                        true 
            | (0, 1) -> 
                // printfn "Moving S"
                match tryFindLargest y intervalsByCol.[x] with 
                | None -> 
                    // printfn "Found no obstacle - exiting. No loop."
                    // route |> List.rev |> List.iter (printfn "%A") 
                    false 
                | Some yLargest -> 
                    // printfn "Found obstacle at %A" (x, yLargest + 1)
                    let nextP = (x, yLargest)
                    // printfn "Move to %A" nextP
                    let movesList = [y .. yLargest] |> List.map (fun i -> ((x, i), dir)) 
                    let moves = movesList |> Set.ofList
                    // printfn "Add moves %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap!"
                        let joined = Set.union moves visited 
                        walk (route @ movesList) joined (turnRight dir) nextP
                    else 
                        // printfn "Overlap found -> there was a loop!"
                        true 
            | (1, 0) -> 
                // printfn "Moving E"
                match tryFindLargest x intervalsByRow.[y] with 
                | None -> 
                    // printfn "Found no obstacle - exiting. No loop."
                    false 
                | Some xLargest -> 
                    // printfn "Found obstacle at %A" (xLargest + 1, y)
                    let nextP = (xLargest, y)
                    // printfn "Move to %A" nextP
                    let movesList = [x .. xLargest] |> List.map (fun i -> ((i, y), dir))
                    let moves = movesList |> Set.ofList
                    // printfn "Add moves %A" moves
                    let overlap = Set.intersect moves visited 
                    if Set.isEmpty overlap then 
                        // printfn "No overlap!"
                        let joined = Set.union moves visited 
                        walk (route @ movesList) joined (turnRight dir) nextP
                    else 
                        // printfn "Overlap found -> there was a loop!"
                        true 
    walk [] Set.empty (0, -1) startPos

let rec insertSorted (n : int) (lst : int list) = 
    match lst with 
    | [] -> [n]
    | a :: rest -> 
        if n < a then n :: a :: rest 
        else a :: (insertSorted n rest)

let createIntervals (obstructions : int list) = 
    match obstructions with 
    | [] -> [ Outside, Outside ]
    | first :: _ -> 
        let last = List.last obstructions 
        let inner = obstructions |> List.pairwise |> List.map (fun (a, b) -> (Obstacle a, Obstacle b))
        [ Outside, Obstacle first ] @ inner @ [ Obstacle last, Outside ]

let hasLoopWithAddedObstruction (obsByRow : int list array) (obsByCol : int list array) startPos (basicBoard : char[,]) (x, y) = 
    // printfn "================"
    let add index value obstructions = 
        let result = Array.copy obstructions
        Array.set result index (insertSorted value result.[index]) 
        result 
    let byRow = obsByRow |> add y x 
    let byCol = obsByCol |> add x y  
    let intervalsByRow = byRow |> Array.map createIntervals
    let intervalsByCol = byCol |> Array.map createIntervals
    let board = addObstruction basicBoard (x, y)
    // printfn "Adding obstruction at %A" (x, y)  
    hasLoop intervalsByRow intervalsByCol startPos board 

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let board = Array2D.fromList lines
    let startPos = findStartPos board
    let obstructions = board |> Array2D.positions |> List.filter (fun (x, y) -> '#' = (Array2D.get board y x)) 
    let obsByRow : (int list) array = 
        board 
        |> Array2D.rowIndexes 
        |> Array.map (fun row -> obstructions |> List.choose (fun (x, y) -> if y = row then Some x else None))
    let obsByCol : (int list) array= 
        board 
        |> Array2D.columnIndexes 
        |> Array.map (fun col -> obstructions |> List.choose (fun (x, y) -> if x = col then Some y else None))
    let intervalsByRow = obsByRow |> Array.map createIntervals
    let intervalsByCol = obsByCol |> Array.map createIntervals

    let visited = board |> patrol startPos
    // Part 1
    visited |> Set.count |> printfn "%d"
    // Part 2
    let sw = Stopwatch.StartNew()
    let route = board |> traceRoute startPos
    // printfn "EXISTING %A" obstructions
    // printfn "ROUTE %A" route
    let allCandidates = visited |> Set.toList 
    // printfn "ALL CANDIDATES %A" allCandidates    

    let extraObs = (3, 6)
    // hasLoopWithAddedObstruction obsByRow obsByCol startPos board extraObs
    //hasLoop intervalsByRow intervalsByCol startPos board
    allCandidates
    |> List.filter (hasLoopWithAddedObstruction obsByRow obsByCol startPos board) 
    |> List.length 
    |> printfn "%d"
    printfn "%A" sw.Elapsed

run "input"
