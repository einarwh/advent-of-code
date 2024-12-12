// Advent of Code 2024. Day 12
// dotnet fsi aoc12.fsx

open System
open System.IO

module Array2D = 
    let inBounds (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        first && second
    let tryGet (a : 'a[,]) (x, y) = 
        if inBounds a (x, y) then Some (Array2D.get a y x) else None
    let positions (a : 'a[,]) = 
        let rowCount = a.GetLength(0)
        let colCount = a.GetLength(1)
        [for x in [0..colCount-1] do for y in [0..rowCount-1] -> (x, y)]
    let fromList (lst : 'a list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let rec fill (garden : char[,]) (ch : char) (x : int, y : int) (plot : Set<int * int>) = 
    if (x, y) |> Array2D.inBounds garden then 
        if Set.contains (x, y) plot then plot 
        else 
            if ch = Array2D.get garden y x then 
                plot 
                |> Set.add (x, y)
                |> fill garden ch ((x - 1), y) 
                |> fill garden ch ((x + 1), y) 
                |> fill garden ch (x, (y - 1)) 
                |> fill garden ch (x, (y + 1))
            else plot 
    else plot 

let fillArea (garden : char[,]) (x, y)  = 
    let ch = Array2D.get garden y x
    fill garden ch (x, y) Set.empty

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let garden = Array2D.fromList lines 
    let foo = fillArea garden (0, 0)
    foo |> printfn "%A"
    let bar = fillArea garden (0, 1)
    bar |> printfn "%A"

run "sample-larger"
