// Advent of Code 2024. Day 15: Warehouse Woes.
// dotnet fsi aoc15.fsx

open System
open System.IO

module Warehouse = 
    let inBounds (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        first && second
    let tryGet (a : 'a[,]) (x, y) = 
        if inBounds a (x, y) then Some (Array2D.get a y x) else None
    let get (a : 'a[,]) (x, y) = 
        Array2D.get a y x
    let set (a : 'a[,]) (x, y) (v : 'a) = 
        Array2D.set a y x v
    let positions (a : 'a[,]) = 
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]
    let map fn (a : 'a[,]) = 
        Array2D.map fn a 
    let fromList (lst : 'a list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)
    let toList (a : 'a[,]) = 
        let yRange = [ 0 .. a.GetLength(0) - 1 ]
        let xRange = [ 0 .. a.GetLength(1) - 1 ]
        yRange 
        |> List.map (fun y -> xRange |> List.map (fun x -> get a (x, y)))

type Pos = int*int

type Move = N | W | S | E 

type Cell = Wall | Box | Space 

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let concat (seq : string seq) = String.Concat(seq)

let join (sep : string) (seq : string seq) = String.Join(sep, seq)

let charToCell ch = 
    match ch with 
    | '#' -> Wall
    | 'O' -> Box 
    | _ -> Space

let cellToChar cell =
    match cell with 
    | Wall -> '#'
    | Box -> 'O' 
    | Space -> '.' 

let visualize warehouse robotPos =
    let charWarehouse = warehouse |> Warehouse.map cellToChar 
    Warehouse.set charWarehouse robotPos '@'
    let lines = charWarehouse |> Warehouse.toList 
    lines |> List.map (fun chars -> new String(List.toArray chars)) |> join "\n" |> printfn "%s"
    // printfn "%A" lines

let charToMove ch = 
    match ch with 
    | '^' -> N 
    | '<' -> W 
    | 'v' -> S 
    | '>' -> E
    | _ -> failwith (sprintf "%c ?" ch)

let parseMoves text = 
    text |> Seq.toList |> List.map charToMove 

let findRobot (warehouse : char[,]) : Pos = 
    let rec find positions = 
        match positions with 
        | [] -> failwith "no robot?"
        | pos :: rest -> 
            if (Warehouse.get warehouse pos) = '@' then pos
            else find rest 
    find <| Warehouse.positions warehouse

let moveToOffset move = 
    match move with 
    | N -> (0, -1)
    | W -> (-1, 0)
    | S -> (0, 1)
    | E -> (1, 0)

let moveStep move (x, y) = 
    let (dx, dy) = moveToOffset move 
    (x + dx, y + dy)

let tryFindSpace (warehouse : Cell[,]) (robotPos : Pos) (move : Move) : (Pos*Pos) list = 
    // printfn "tryFindSpace with robot in pos %A: %A" robotPos move
    let rec loop (x, y) (swaps : (Pos*Pos) list) = 
        let (dx, dy) = moveToOffset move 
        let pos = (x + dx, y + dy)
        let nextSwaps = (pos, (x, y)) :: swaps 
        match Warehouse.get warehouse pos with 
        | Wall -> 
            // printfn "Hit wall."
            [] 
        | Space ->
            // printfn "Found space!"
            nextSwaps
        | Box ->
            // printfn "Found box... keep looking." 
            loop pos nextSwaps
    loop robotPos []

let rec moveStuff warehouse swaps = 
    match swaps with 
    | [] -> ()
    | (pos1, pos2) :: rest -> 
        // printfn "Swapping."
        let cell1 = Warehouse.get warehouse pos1 
        let cell2 = Warehouse.get warehouse pos2
        Warehouse.set warehouse pos1 cell2 
        Warehouse.set warehouse pos2 cell1 
        moveStuff warehouse rest 

let tryMoveRobot (warehouse : Cell[,], robotPos : Pos) (move : Move) = 
    // printfn "tryMoveRobot in pos %A: %A" robotPos move
    match tryFindSpace warehouse robotPos move with 
    | [] -> (warehouse, robotPos)
    | swaps -> 
        moveStuff warehouse swaps 
        (warehouse, moveStep move robotPos)

let rec makeMoves (warehouse : Cell[,], robotPos : Pos) (moves : Move list) = 
    // printfn "\n"
    // visualize warehouse robotPos
    match moves with 
    | [] -> (warehouse, robotPos)
    | m :: restMoves -> 
        let result = tryMoveRobot (warehouse, robotPos) m 
        makeMoves result restMoves

let gpsCoordinate (x, y) = 
    y * 100 + x

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let toLines = split "\n" >> Array.toList
    let joinUp = split "\n" >> concat
    let text0 = text.[0]
    let text1 = text.[1]
    let lines = text0 |> toLines |> List.map Seq.toList 
    let charWarehouse = lines |> Warehouse.fromList
    let robotPos = findRobot charWarehouse
    let warehouse = charWarehouse |> Warehouse.map (charToCell)
    let moves = text1 |> joinUp |> parseMoves 
    let (wh, rp) = makeMoves (warehouse, robotPos) moves 
    let boxPositions = wh |> Warehouse.positions |> List.choose (fun p -> if Warehouse.get wh p = Box then Some p else None)
    boxPositions
    |> List.sumBy gpsCoordinate
    |> printfn "%d"
    0

run "input"
