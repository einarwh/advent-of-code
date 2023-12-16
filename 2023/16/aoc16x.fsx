// Advent of Code 2023. Day 16: The Floor Will Be Lava
// dotnet fsi aoc16.fsx

open System
open System.IO

module Array2D =
    let getRowCount (ary : 'a[,]) = ary.GetLength(0)
    let getColumnCount (ary : 'a[,]) = ary.GetLength(1)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let step (x0, y0) (x, y) =
    (x + x0, y + y0)

let rec reflect pos dir grid seen =
    let (x, y) = pos
    let xLimit = Array2D.getColumnCount grid
    let yLimit = Array2D.getRowCount grid
    if x < 0 || x >= xLimit || y < 0 || y >= yLimit || Set.contains (pos, dir) seen then
        seen
    else 
        let seen = seen |> Set.add (pos, dir)
        match Array2D.get grid y x with
        | '|' when dir = (-1, 0) || dir = (1, 0) ->
            seen |> reflect (step (0, -1) pos) (0, -1) grid |> reflect (step (0, 1) pos) (0, 1) grid 
        | '-' when dir = (0, -1) || dir = (0, 1) ->
            seen |> reflect (step (-1, 0) pos) (-1, 0) grid |> reflect (step (1, 0) pos) (1, 0) grid
        | '/' ->
            let nextDir (x, y) = (-y, -x)
            reflect (step dir pos) (nextDir dir) grid seen
        | '\\' ->
            let nextDir (x, y) = (y, x)
            reflect (step dir pos) (nextDir dir) grid seen
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
    let north = nStarts |> findBest grid (0, 1)
    let west = wStarts |> findBest grid (1, 0) 
    let south = sStarts |> findBest grid (0, -1)
    let east = eStarts |> findBest grid (-1, 0)
    [ north; west; south; east ] |> List.max 
    
let run fileName =
    let lines = readLines fileName
    let grid = array2D lines
    let seen = reflect (0, 0) (1, 0) grid Set.empty
    seen |> Set.map fst |> Set.count |> printfn "%d"
    solve grid |> printfn "%d"

"input" |> run