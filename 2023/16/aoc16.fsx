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

let rec reflect (pos : int*int) (dir : Dir) (grid : char[,]) seen =
    let (x, y) = pos
    let xLimit = Array2D.getColumnCount grid
    let yLimit = Array2D.getRowCount grid
    if Set.contains (pos, dir) seen then
        seen
    elif x >= 0 && x < xLimit && y >= 0 && y < yLimit then
        let seen = seen |> Set.add (pos, dir)
        let ch = Array2D.get grid y x
        match ch with
        | '.' ->
            let nextPos = continueInDir dir pos
            reflect nextPos dir grid seen
        | '|' when dir = N || dir = S ->
            let nextPos = continueInDir dir pos
            reflect nextPos dir grid seen
        | '|' ->
            let northPos = continueInDir N pos
            let southPos = continueInDir S pos
            seen |> reflect northPos N grid |> reflect southPos S grid 
        | '-' when dir = W || dir = E ->
            let nextPos = continueInDir dir pos
            reflect nextPos dir grid seen
        | '-' ->
            let westPos = continueInDir W pos
            let eastPos = continueInDir E pos
            seen |> reflect westPos W grid |> reflect eastPos E grid
        | '/' ->
            let newDir =
                match dir with
                | N -> E
                | W -> S
                | S -> W
                | E -> N
            let nextPos = continueInDir newDir pos
            reflect nextPos newDir grid seen
        | '\\' ->
            let newDir =
                match dir with
                | N -> W
                | W -> N
                | S -> E
                | E -> S
            let nextPos = continueInDir newDir pos
            reflect nextPos newDir grid seen
        | _ -> failwith <| sprintf "%c?" ch
    else
        seen

let findBest grid dir startPositions = 
    startPositions 
    |> List.map (fun pos -> reflect pos dir grid Set.empty)
    |> List.map (Set.map fst)
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
    let seen = reflect (0, 0) E grid Set.empty
    seen |> Set.map fst |> Set.count |> printfn "%d"
    solve grid |> printfn "%d"

"input" |> run