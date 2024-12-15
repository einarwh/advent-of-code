// Advent of Code 2024. Day 15: Warehouse Woes.
// dotnet fsi aoc15.fsx

open System
open System.IO

module Warehouse = 
    let inBounds (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        first && second
    let tryGet (a : 'a[,]) (x, y) = 
        if inBounds a (x, y) then Some (Array2D.get a y x) else None
    let get (a : 'a[,]) (x, y) = 
        Array2D.get a y x
    let positions (a : 'a[,]) = 
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]
    let fromList (lst : 'a list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)

type Move = N | W | S | E 

type Cell = Wall | Box | Space 

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let concat (seq : string seq) = String.Concat(seq)

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let toLines = split "\n" >> Array.toList
    let joinUp = split "\n" >> concat
    let text0 = text.[0]
    let text1 = text.[1]
    let lines = text0 |> toLines
    lines |> printfn "%A"
    let commandText = text1 |> joinUp
    commandText |> printfn "%A"

run "sample-small"
