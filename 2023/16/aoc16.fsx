// Advent of Code 2023. Day 16: The Floor Will Be Lava
// dotnet fsi aoc16.fsx

open System
open System.IO

type Dir = N | W | S | E

module Array2D =

    let getRowCount (ary : 'a[,]) = ary.GetLength(0)

    let getColumnCount (ary : 'a[,]) = ary.GetLength(1)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let continueInDir dir (x, y) =
    match dir with
    | N -> (x, y - 1)
    | W -> (x - 1, y)
    | S -> (x, y + 1)
    | E -> (x + 1, y)

let rec reflect (pos : int*int) (dir : Dir) (grid : char[,]) energized seen =
    let (x, y) = pos
    let xLimit = Array2D.getColumnCount grid
    let yLimit = Array2D.getRowCount grid
    if Set.contains (pos, dir) seen then
        (energized, seen)
    elif x >= 0 && x < xLimit && y >= 0 && y < yLimit then
        let energized = energized |> Set.add pos
        let seen = seen |> Set.add (pos, dir)
        let ch = Array2D.get grid y x
        match ch with
        | '.' ->
            // Continue in same direction.
            let nextPos = continueInDir dir pos
            reflect nextPos dir grid energized seen
        | '|' when dir = N || dir = S ->
            // Continue in same direction.
            let nextPos = continueInDir dir pos
            reflect nextPos dir grid energized seen
        | '|' ->
            // Split in directions N and S.
            let northPos = continueInDir N pos
            let southPos = continueInDir S pos
            let (energized, seen) = reflect northPos N grid energized seen
            let (energized, seen) = reflect southPos S grid energized seen
            (energized, seen)
        | '-' when dir = W || dir = E ->
            // Continue in same direction.
            let nextPos = continueInDir dir pos
            reflect nextPos dir grid energized seen
        | '-' ->
            // Split in directions W and E.
            let westPos = continueInDir W pos
            let eastPos = continueInDir E pos
            let (energized, seen) = reflect westPos W grid energized seen
            let (energized, seen) = reflect eastPos E grid energized seen
            (energized, seen)
        | '/' ->
            // Continue in reflected direction.
            let newDir =
                match dir with
                | N -> E
                | W -> S
                | S -> W
                | E -> N
            let nextPos = continueInDir newDir pos
            reflect nextPos newDir grid energized seen
        | '\\' ->
            // Continue in reflected direction.
            let newDir =
                match dir with
                | N -> W
                | W -> N
                | S -> E
                | E -> S
            let nextPos = continueInDir newDir pos
            reflect nextPos newDir grid energized seen
        | _ -> failwith <| sprintf "%c?" ch
    else
        (energized, seen)

let findBest grid dir startPositions = 
    startPositions 
    |> List.map (fun pos -> reflect pos dir grid Set.empty Set.empty)
    |> List.map fst 
    |> List.map Set.count
    |> List.sortDescending
    |> List.head 

let solve grid = 
    let xLimit = Array2D.getColumnCount grid
    let yLimit = Array2D.getRowCount grid
    let nStarts = [0 .. xLimit - 1] |> List.map (fun x -> (x, 0))
    let sStarts = [0 .. xLimit - 1] |> List.map (fun x -> (x, yLimit - 1))
    let wStarts = [0 .. yLimit - 1] |> List.map (fun y -> (0, y))
    let eStarts = [0 .. yLimit - 1] |> List.map (fun y -> (xLimit - 1, y))
    let north = nStarts |> findBest grid S
    let west = wStarts |> findBest grid E 
    let south = sStarts |> findBest grid N 
    let east = eStarts |> findBest grid W
    let candidates = [ north; west; south; east ]
    candidates |> List.sortDescending |> List.head 

let run fileName =
    let lines = readLines fileName
    let grid = array2D lines
    let (energized, _) = reflect (0, 0) E grid Set.empty Set.empty
    energized |> Set.count |> printfn "%d"
    solve grid |> printfn "%d"

"input" |> run