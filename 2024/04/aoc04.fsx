// Advent of Code 2024. Day 04
// dotnet fsi aoc04.fsx

open System
open System.IO

module Array2D = 
    let tryGet (a : 'a[,]) index1 index2 = 
        let first = index1 >= 0 && index1 < a.GetLength(0)
        let second = index2 >= 0 && index2 < a.GetLength(1)
        if first && second then 
            Some <| Array2D.get a index1 index2
        else 
            None 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let width = lines |> List.head |> Seq.length 
    let height = lines |> List.length 
    let board = Array2D.init height width (fun i j -> lines.[i].[j])
    board |> printfn "%A"

run "sample"
