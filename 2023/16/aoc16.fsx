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

let step dir (x, y) =
    match dir with
    | N -> (x, y - 1)
    | W -> (x - 1, y)
    | S -> (x, y + 1)
    | E -> (x + 1, y)

let rec reflect pos dir grid seen =
    let (x, y) = pos
    let xLimit = Array2D.getColumnCount grid
    let yLimit = Array2D.getRowCount grid
    if x < 0 || x >= xLimit || y < 0 || y >= yLimit || Set.contains (pos, dir) seen then
        seen
    else 
        let seen = seen |> Set.add (pos, dir)
        match Array2D.get grid y x with
        | '|' when dir = N || dir = S ->
            reflect (step dir pos) dir grid seen
        | '|' ->
            seen |> reflect (step N pos) N grid |> reflect (step S pos) S grid 
        | '-' when dir = W || dir = E ->
            reflect (step dir pos) dir grid seen
        | '-' ->
            seen |> reflect (step W pos) W grid |> reflect (step E pos) E grid
        | '/' ->
            let dir =
                match dir with
                | N -> E
                | W -> S
                | S -> W
                | E -> N
            reflect (step dir pos) dir grid seen
        | '\\' ->
            let dir =
                match dir with
                | N -> W
                | W -> N
                | S -> E
                | E -> S
            reflect (step dir pos) dir grid seen
        | _ ->
            reflect (step dir pos) dir grid seen

let findBest grid dir startPositions = 
    startPositions 
    |> List.map (fun pos -> reflect pos dir grid Set.empty)
    |> List.map (Set.map fst >> Set.count)
    |> List.max
 
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
    [ north; west; south; east ] |> List.max 
    
let run fileName =
    let lines = readLines fileName
    let grid = array2D lines
    let seen = reflect (0, 0) E grid Set.empty
    seen |> Set.map fst |> Set.count |> printfn "%d"
    solve grid |> printfn "%d"

"input" |> run