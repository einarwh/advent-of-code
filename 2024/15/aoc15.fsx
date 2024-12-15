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
    let map fn (a : 'a[,]) = 
        Array2D.map fn a 
    let fromList (lst : 'a list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)
    let toList (a : 'a[,]) = 
        let yRange = [ 0 .. a.GetLength(0) - 1 ]
        let xRange = [ 0 .. a.GetLength(1) - 1 ]
        yRange 
        |> List.map (fun y -> xRange |> List.map (fun x -> get a (x, y)))

type Move = N | W | S | E 

type Cell = Wall | Box | Space 

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let concat (seq : string seq) = String.Concat(seq)

let join (sep : string) (seq : string seq) = String.Join(sep, seq)

let charToCell ch = 
    match ch with 
    | '#' -> Wall
    | 'O' -> Box 
    | _ -> Space

let cellToChar cell =
    match cell with 
    | Wall -> '#'
    | Box -> 'O' 
    | Space -> '.' 

let visualize warehouse = 
    let lines = warehouse |> Warehouse.map cellToChar |> Warehouse.toList 
    lines |> List.map (fun chars -> new String(List.toArray chars)) |> join "\n" |> printfn "%s"
    // printfn "%A" lines

let charToCommand ch = 
    match ch with 
    | '^' -> N 
    | '<' -> W 
    | 'v' -> S 
    | '>' -> E
    | _ -> failwith (sprintf "%c ?" ch)

let parseCommands text = 
    text |> Seq.toList |> List.map charToCommand 

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let toLines = split "\n" >> Array.toList
    let joinUp = split "\n" >> concat
    let text0 = text.[0]
    let text1 = text.[1]
    let lines = text0 |> toLines |> List.map Seq.toList 
    let warehouse = lines |> Warehouse.fromList |> Warehouse.map (charToCell)
    visualize warehouse
    let commands = text1 |> joinUp |> parseCommands 
    commands |> printfn "%A"

run "sample-small"
