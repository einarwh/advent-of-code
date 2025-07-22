// Advent of Code 2015. Day 18: Like a GIF For Your Yard.
// dotnet fsi aoc18.fsx

open System
open System.IO

module Grid = 
    let create width height = 
        Array2D.create height width 0
    let width grid = 
        Array2D.length2 grid
    let height grid = 
        Array2D.length1 grid
    let get (grid : bool[,]) (x, y) =
        Array2D.get grid y x
    let set (grid : bool[,]) (x, y) value =
        Array2D.set grid y x value
    let count (grid : bool[,]) = 
        let w = width grid
        let h = height grid
        let posList = [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        posList |> List.filter (fun pos -> get grid pos) |> List.length
    let fromNestedList (lst : bool list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)
    let toNestedList (grid : bool[,]) =
        let yRange = [ 0 .. grid.GetLength(0) - 1 ]
        let xRange = [ 0 .. grid.GetLength(1) - 1 ]
        yRange
        |> List.map (fun y -> xRange |> List.map (fun x -> get grid (x, y)))

let visualize grid =
    let lines = grid |> Grid.toNestedList
    lines 
    |> List.map (fun values -> values |> List.map (fun v -> if v then '#' else '.'))
    |> List.map (fun chars -> new String(List.toArray chars)) 
    |> String.concat "\n" 
    |> printfn "%s"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let grid = 
        lines |> List.map (fun s -> s |> Seq.toList |> List.map (fun ch -> ch = '#')) |> Grid.fromNestedList
    grid |> visualize
    grid |> Grid.count |> printfn "%d"

run "sample.txt"
