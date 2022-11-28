// Advent of Code 2021. Day 9, part A. 
// dotnet fsi aoc09a.fsx

open System
open System.IO

let parseNumbers (s : string) : int array = 
    s 
    |> Seq.map (fun c -> Char.ToString(c) |> int) 
    |> Seq.toArray 

let lookup (x : int) (y : int) (map : int[,]) : int option = 
    let xMax = Array2D.length1 map 
    let yMax = Array2D.length2 map 
    if x >= 0 && x < xMax && y >= 0 && y < yMax then 
        Some (Array2D.get map x y)
    else 
        None

let neighbours (x : int) (y : int) (map : int[,]) : int list = 
    [ lookup x (y - 1) map 
      lookup x (y + 1) map
      lookup (x - 1) y map 
      lookup (x + 1) y map ]
    |> List.choose id     

let isLowPoint (map : int[,]) (x : int) (y : int) : bool = 
    let depth = Array2D.get map x y 
    neighbours x y map |> List.forall (fun d -> depth < d) 

let run (map : int[,]) = 
    let lowPoints = 
        map 
        |> Array2D.mapi (fun x y it -> if isLowPoint map x y then Some it else None) 
        |> Seq.cast<int option> 
        |> Seq.choose id 
        |> Seq.toList 
    let riskLevelSum = 
        lowPoints |> List.map (fun d -> d + 1) |> List.sum
    printfn "Risk level sum: %d" riskLevelSum
    
"input"
|> File.ReadAllLines
|> Array.filter (fun s -> s.Length > 0)
|> Array.map parseNumbers
|> array2D
|> run 
