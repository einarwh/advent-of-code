// Advent of Code 2017. Day 17: Conway Cubes. Part 1.
// dotnet fsi aoc17a.fsx

open System.IO

type Cell = int*int*int
type Grid = Map<Cell, bool>

module Grid =
    
    let empty : Grid = Map.empty
    
    let add (grid : Grid) (cell : Cell) : Grid =
        grid |> Map.add cell true

    let initWith (cells : Cell list) : Grid =
        cells |> List.fold add empty 
            
    let cells (grid : Grid) : Cell list =
        grid |> Map.toList |> List.map fst

    let private isActive (grid : Grid) (cell : Cell) =
        Map.containsKey cell grid
            
    let private allNeighbors (x, y, z) =
        [ for xi in [x-1 .. x+1] do
            for yi in [y-1 .. y+1] do 
                for zi in [z-1 .. z+1] -> (xi, yi, zi) ]
        |> List.filter ((<>) (x, y, z))
            
    let private liveNeighbors (grid : Grid) (cell : Cell) : Cell list =
        allNeighbors cell |> List.filter (isActive grid)
        
    let private evolveCell (grid : Grid) (cell : Cell) : Cell option =
        let active = isActive grid cell 
        let activeCount = liveNeighbors grid cell |> List.length
        if active then
            if activeCount = 2 || activeCount = 3 then Some cell else None
        else
            if activeCount = 3 then Some cell else None
    
    let evolve (grid : Grid) : Grid =
        grid
        |> cells 
        |> List.collect allNeighbors
        |> List.distinct
        |> List.choose (evolveCell grid)
        |> initWith
        
    let count grid : int =
        Map.count grid

let readCells (y : int, line : string) : Cell list =
    line
    |> Seq.toList
    |> List.mapi (fun x c -> (x, c))
    |> List.choose (fun (x, c) -> if c = '#' then Some (x, y, 0) else None) 

let rec times n fn =
    if n < 1 then id
    else fn >> (times (n-1) fn)
        
let run fileName =
    let lines = File.ReadAllLines fileName |> Array.toList |> List.filter (fun s -> s.Length > 0)
    let startCells = lines |> List.mapi (fun i line -> readCells (i, line)) |> List.collect id
    let grid =
        startCells
        |> Grid.initWith
        |> times 6 Grid.evolve
    printfn "Cells: %d" (Grid.count grid)

run "input.txt"