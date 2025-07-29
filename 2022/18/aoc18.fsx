// Advent of Code 2022. Day 18: Boiling Boulders.
// dotnet fsi aoc18.fsx

open System
open System.IO

let parse (s : string) = 
    let nums = s.Split "," |> Array.map int 
    nums[0], nums[1], nums[2]

let getManhattanDistance (x1,y1,z1) (x2,y2,z2) = 
    abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

let countNeighbours positions pos = 
    positions |> List.filter (fun p -> 1 = getManhattanDistance p pos) |> List.length 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let positions = lines |> List.map parse 
    let totalNeighbours = positions |> List.map (countNeighbours positions) |> List.sum 
    totalNeighbours |> printfn "%d"
    let surface = 6 * List.length positions - totalNeighbours
    surface |> printfn "%d"

run "input.txt"
