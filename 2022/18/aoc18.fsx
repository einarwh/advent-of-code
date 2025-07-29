// Advent of Code 2022. Day 18: Boiling Boulders.
// dotnet fsi aoc18.fsx

open System
open System.IO

type Pos = int*int*int

let parse (s : string) = 
    let nums = s.Split "," |> Array.map int 
    nums[0], nums[1], nums[2]

let getManhattanDistance (x1,y1,z1) (x2,y2,z2) = 
    abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

let getCubeNeighbours (x, y, z) = 
    [ (x - 1, y, z)
      (x + 1, y, z)
      (x, y - 1, z)
      (x, y + 1, z)
      (x, y, z - 1)
      (x, y, z + 1) ]

let insideBounds (xMin, yMin, zMin) (xMax, yMax, zMax) (x, y, z) =
    x >= xMin && x <= xMax && y >= yMin && y <= yMax && z >= zMin && z <= zMax 

let excluded inside cubes pos = 
    Set.contains pos inside || Set.contains pos cubes 

let floodFill startPos positions : Set<Pos>= 
    let cubes = positions |> Set.ofList
    let xs = positions |> List.map (fun (x,_,_) -> x)
    let ys = positions |> List.map (fun (_,y,_) -> y)
    let zs = positions |> List.map (fun (_,_,z) -> z)
    let minMax vs = (vs |> List.min) - 1, (vs |> List.max) + 1
    let xMin, xMax = minMax xs
    let yMin, yMax = minMax ys 
    let zMin, zMax = minMax zs 
    let rec flood (inside : Set<Pos>) (x, y, z) = 
        let inside' = Set.add (x, y, z) inside 
        let filtered = 
            (x, y, z) 
            |> getCubeNeighbours 
            |> List.filter (insideBounds (xMin, yMin, zMin) (xMax, yMax, zMax))
            |> List.filter (not << excluded inside cubes)
        filtered |> List.fold (fun s pos -> flood s pos) inside' 
    flood Set.empty startPos

let countSurfaceEdges positions pos = 
    positions |> List.filter (fun p -> 1 = getManhattanDistance p pos) |> List.length 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let positions = lines |> List.map parse 
    let totalNeighbours = positions |> List.map (countSurfaceEdges positions) |> List.sum 
    let surface = 6 * List.length positions - totalNeighbours
    surface |> printfn "%d"
    let fill = floodFill (0,0,0) positions |> Set.toList
    positions |> List.map (countSurfaceEdges fill) |> List.sum |> printfn "%d"

run "input.txt"
