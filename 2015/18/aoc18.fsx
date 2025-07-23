// Advent of Code 2015. Day 18: Like a GIF For Your Yard.
// dotnet fsi aoc18.fsx

open System
open System.IO

module Grid =
    let width grid =
        Array2D.length2 grid
    let height grid =
        Array2D.length1 grid
    let isCorner grid (x, y) = 
        let xMax = width grid - 1
        let yMax = height grid - 1
        [(0, 0); (0, yMax); (xMax, 0); (xMax, yMax)] |> List.contains (x, y)
    let get (grid : bool[,]) (x, y) =
        Array2D.get grid y x
    let tryGet (grid : bool[,]) (x, y) =
        if x < 0 || x >= width grid || y < 0 || y >= height grid then
            None
        else
            Some (get grid (x, y))
    let set (grid : bool[,]) (x, y) value =
        Array2D.set grid y x value
    let getPositions (grid : bool[,]) =
        let w = width grid
        let h = height grid
        [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
    let count (grid : bool[,]) =
        let posList = getPositions grid
        posList |> List.filter (fun pos -> get grid pos) |> List.length
    let getNeighbours (grid : bool[,]) (x, y) =
        let positions = [(x-1, y-1); (x, y-1); (x+1, y-1); (x-1,y); (x+1,y); (x-1, y+1); (x, y+1); (x+1, y+1)]
        positions |> List.choose (tryGet grid)
    let fromNestedList (lst : bool list list) =
        let width = lst |> List.head |> List.length
        let height = lst |> List.length
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)
    let toNestedList (grid : bool[,]) =
        let yRange = [ 0 .. grid.GetLength(0) - 1 ]
        let xRange = [ 0 .. grid.GetLength(1) - 1 ]
        yRange
        |> List.map (fun y -> xRange |> List.map (fun x -> get grid (x, y)))

let step cornersAlwaysOn grid =
    let nextGrid = Array2D.copy grid
    let posList = Grid.getPositions grid
    let turnOn (pos : int*int) =
        if cornersAlwaysOn && Grid.isCorner grid pos then true 
        else 
            let nbCount = Grid.getNeighbours grid pos |> List.filter id |> List.length
            let currentlyOn = Grid.get grid pos
            (currentlyOn && (nbCount = 2 || nbCount = 3)) || (not currentlyOn && nbCount = 3)
    posList |> List.iter (fun pos -> Grid.set nextGrid pos (turnOn pos))
    nextGrid

let visualize grid =
    let lines = grid |> Grid.toNestedList
    lines
    |> List.map (fun values -> values |> List.map (fun v -> if v then '#' else '.'))
    |> List.map (fun chars -> new String(List.toArray chars))
    |> String.concat "\n"
    |> printfn "%s"

let animate cornersAlwaysOn steps (grid : bool[,]) = 
    let rec loop stepsLeft (g : bool[,]) = 
        // visualize g 
        if stepsLeft > 0 then 
            loop (stepsLeft - 1) (step cornersAlwaysOn g)
        else 
            g
    let g = Array2D.copy grid 
    if cornersAlwaysOn then 
        let xMax = Grid.width grid - 1
        let yMax = Grid.height grid - 1
        [(0, 0); (xMax, 0); (0, yMax); (xMax, yMax)] |> List.iter (fun pos -> Grid.set g pos true) 
    loop steps g

let readLines =
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName =
    let lines = readLines fileName
    let grid =
        lines |> List.map (fun s -> s |> Seq.toList |> List.map (fun ch -> ch = '#')) |> Grid.fromNestedList
    grid |> animate false 100 |> Grid.count |> printfn "%d"
    grid |> animate true 100 |> Grid.count |> printfn "%d"

run "input.txt"
