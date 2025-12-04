// Advent of Code 2025. Day 04: Printing Department.
// dotnet fsi aoc04.fsx

open System
open System.IO

module Grid =
    let width grid =
        Array2D.length2 grid
    let height grid =
        Array2D.length1 grid
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
    let getNeighbours (grid : bool[,]) (x, y) =
        let positions = [(x-1, y-1); (x, y-1); (x+1, y-1); (x-1,y); (x+1,y); (x-1, y+1); (x, y+1); (x+1, y+1)]
        positions |> List.choose (tryGet grid)
    let getRollCount (grid : bool[,]) (x, y) =
        getNeighbours grid (x, y) |> List.filter id |> List.length
    let fromNestedList (lst : bool list list) =
        let width = lst |> List.head |> List.length
        let height = lst |> List.length
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let findRemovable grid = 
    grid 
    |> Grid.getPositions
    |> List.filter (fun pos -> Grid.get grid pos && Grid.getRollCount grid pos < 4)

let solve grid =
    let rec loop removed = 
        let removable = findRemovable grid 
        if List.isEmpty removable then 
            removed |> List.length
        else 
            removable |> List.iter (fun pos -> Grid.set grid pos false) 
            loop (removable @ removed) 
    loop []

let run fileName = 
    let lines = readLines fileName
    let grid =
        lines |> List.map (fun s -> s |> Seq.toList |> List.map (fun ch -> ch = '@')) |> Grid.fromNestedList
    grid 
    |> findRemovable
    |> List.length 
    |> printfn "%d"
    grid 
    |> solve
    |> printfn "%d"

run "input.txt"
