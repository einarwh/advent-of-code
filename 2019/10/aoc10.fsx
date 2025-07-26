// Advent of Code 2019. Day 10: Monitoring Station.
// dotnet fsi aoc10.fsx

open System
open System.IO

module Grid =
    let get (grid : bool[,]) (x, y) =
        Array2D.get grid y x
    let set (grid : bool[,]) (x, y) (value : bool) =
        Array2D.set grid y x value
    let fromList (lst : bool list list) =
        let width = lst |> List.head |> List.length
        let height = lst |> List.length
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)
    let getPositions (grid : bool[,]) =
        let yRange = [ 0 .. grid.GetLength(0) - 1 ]
        let xRange = [ 0 .. grid.GetLength(1) - 1 ]
        yRange |> List.collect (fun y -> xRange |> List.map (fun x -> x, y))
    let toNestedList (grid : bool[,]) =
        let yRange = [ 0 .. grid.GetLength(0) - 1 ]
        let xRange = [ 0 .. grid.GetLength(1) - 1 ]
        yRange
        |> List.map (fun y -> xRange |> List.map (fun x -> get grid (x, y)))
    let toIndexedList (grid : bool[,]) =
        let yRange = [ 0 .. grid.GetLength(0) - 1 ]
        let xRange = [ 0 .. grid.GetLength(1) - 1 ]
        yRange
        |> List.map (fun y -> xRange |> List.map (fun x -> (x, y), get grid (x, y)))
        |> List.concat

let rec gcd a b =
    // printfn "gcd a=%d b=%d" a b
    if b = 0 then a else gcd b (a % b)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let findVectorHlp (ax, ay) (x, y) = 
    let dx = x - ax 
    let dy = y - ay 
    match dx, dy with 
    | (0, 0) -> failwith "?"
    | (_, 0) -> (- dx / abs dx, 0)
    | (0, _) -> (0, - dy / abs dy)
    | _ -> 
        let d = gcd dx dy 
        let result = - dx / abs d, - dy / abs d
        // printfn "divisor: %d" d
        result 

let findVector (ax, ay) (x, y) = 
    let result = findVectorHlp (ax, ay) (x, y) 
    // printfn "findVector: %A %A -> %A" (ax, ay) (x, y) result 
    result

let countVisibleAsteroids (asteroidPositions : (int*int) list) (pos : int*int) = 
    let grouped = 
        asteroidPositions 
        |> List.filter (fun p -> p <> pos)
        |> List.map (fun p -> (p, findVector p pos))
        |> List.groupBy snd 
        |> List.map (fun (v, lst) -> (v, List.map fst lst))
    // printfn ""
    // printfn "pos: %A - visible: %A" pos (List.length grouped)
    // mapped |> List.iter (printfn "%A")
    // printfn "..."
    // grouped |> List.iter (printfn "%A")
    grouped
    |> List.length 

let run fileName = 
    let lines = readLines fileName
    let grid = lines |> List.map Seq.toList |> List.map (List.map (fun ch -> ch = '#')) |> Grid.fromList
    let positions = Grid.getPositions grid 
    let asteroidPositions = positions |> List.filter (Grid.get grid)
    let bestPlace, count = 
        asteroidPositions 
        |> List.map (fun p -> (p, countVisibleAsteroids asteroidPositions p)) 
        |> List.sortByDescending snd
        |> List.head
    printfn "%A: %d" bestPlace count 
    // let pos = (0, 0) 
    // findVector pos (1, 1) |> printfn "%A"
    // findVector pos (-1, -1) |> printfn "%A"
    Math.Atan2 (1.0, 0) |> printfn "(1.0, 0) : %f"
    Math.Atan2 (0.5, 0.5) |> printfn "(0.5, 0.5) : %f"
    Math.Atan2 (0, 1.0) |> printfn "(0, 1.0) : %f"
    Math.Atan2 (-0.5, 0.5) |> printfn "(-0.5, 0.5) : %f"
    Math.Atan2 (-1.0, 0) |> printfn "(-1.0, 0) : %f"
    Math.Atan2 (-0.5, -0.5) |> printfn "(-0.5, -0.5) : %f"
    Math.Atan2 (0, -1.0) |> printfn "(0, -1.0) : %f"
    Math.Atan2 (1.0, 0) |> printfn "(1.0, 0) : %f"

    // Math.Atan2 (0, 1.0) |> printfn ">>> (0, 1) : %f"


    0


run "input.txt"
