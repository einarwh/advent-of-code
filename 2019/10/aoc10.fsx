// Advent of Code 2019. Day 10: Monitoring Station.
// dotnet fsi aoc10.fsx

open System
open System.IO

module Grid =
    let get (grid : bool[,]) (x, y) =
        Array2D.get grid y x
    let fromList (lst : bool list list) =
        let width = lst |> List.head |> List.length
        let height = lst |> List.length
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)
    let getPositions (grid : bool[,]) =
        let yRange = [ 0 .. grid.GetLength(0) - 1 ]
        let xRange = [ 0 .. grid.GetLength(1) - 1 ]
        yRange |> List.collect (fun y -> xRange |> List.map (fun x -> x, y))

let rec gcd a b =
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
        - dx / abs d, - dy / abs d

let findVector (ax, ay) (x, y) = 
    let result = findVectorHlp (ax, ay) (x, y) 
    result

let countVisibleAsteroids (asteroidPositions : (int*int) list) (pos : int*int) = 
    asteroidPositions 
    |> List.filter (fun p -> p <> pos)
    |> List.map (fun p -> (p, findVector p pos))
    |> List.groupBy snd 
    |> List.map (fun (v, lst) -> (v, List.map fst lst))
    |> List.length

let findAngle ((ax, ay) : (int*int)) ((x, y) : (int*int)) : (int*int)*float*float = 
    let dx = float ax - float x 
    let dy = float ay - float y 
    let dist = Math.Sqrt(dx*dx + dy*dy)
    let angle = Math.Atan2(dx, -dy)
    (x, y), angle, dist

let toVaporizationOrder (asteroidPositions : (int*int) list) (pos : int*int) = 
    let rec fn acc groups =
        match groups with 
        | [] -> acc |> List.rev |> List.concat
        | _ -> 
            let heads = groups |> List.map List.head 
            let remaining = groups |> List.map List.tail |> List.filter (not << List.isEmpty)
            fn (heads :: acc) remaining
    let grouped = 
        asteroidPositions 
        |> List.filter (fun p -> p <> pos)
        |> List.map (findAngle pos)
        |> List.groupBy (fun (p, a, d) -> a)
        |> List.map (fun (a, lst) -> ((if a = Math.PI then -a else a), lst |> List.map (fun (p, a, d) -> (p, d)) |> List.sortBy snd |> List.map fst))
        |> List.sort
        |> List.map snd
    fn [] grouped
    
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
    printfn "%d" count
    let order = bestPlace |> toVaporizationOrder asteroidPositions
    let x, y = order |> List.skip 199 |> List.head 
    100 * x + y |> printfn "%d"

run "input.txt"
