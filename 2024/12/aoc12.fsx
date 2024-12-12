// Advent of Code 2024. Day 12: Garden Groups.
// dotnet fsi aoc12.fsx

open System
open System.IO

module Garden = 
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

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let rec fill (garden : char[,]) (ch : char) (x : int, y : int) (plot : Set<int * int>) = 
    if (x, y) |> Garden.inBounds garden then 
        if Set.contains (x, y) plot then plot 
        else 
            if ch = Garden.get garden (x, y) then 
                plot 
                |> Set.add (x, y)
                |> fill garden ch ((x - 1), y) 
                |> fill garden ch ((x + 1), y) 
                |> fill garden ch (x, (y - 1)) 
                |> fill garden ch (x, (y + 1))
            else plot 
    else plot 

let fillPlot (garden : char[,]) (x, y)  = 
    let ch = Garden.get garden (x, y)
    fill garden ch (x, y) Set.empty

let findPlots (garden : char[,]) =
    let rec loop (startPositions : (int*int) list) (plots : Set<int*int> list) (visited : Set<int*int>) = 
        match startPositions with 
        | [] -> plots 
        | pos :: remaining -> 
            if Set.contains pos visited then 
                loop remaining plots visited 
            else 
                let plot = fillPlot garden pos 
                loop remaining (plot :: plots) (Set.union visited plot)
    let startPositions = Garden.positions garden 
    loop startPositions [] Set.empty

let calculateArea plot = Set.count plot 

let borders (garden : char[,]) (x, y) : int = 
    let isBorder plotPlant pos = 
        match Garden.tryGet garden pos with 
        | None -> true 
        | Some plant -> plant <> plotPlant
    let plotPlant = Garden.get garden (x, y)
    [ (x, y-1); (x-1, y); (x, y+1); (x+1, y) ]
    |> List.filter (isBorder plotPlant)
    |> List.length 

let calculatePerimeter (garden : char[,]) (plot : Set<int*int>) = 
    plot 
    |> Set.toList 
    |> List.sumBy (borders garden) 

let fenceCost (garden : char[,]) (plot : Set<int*int>) = 
    let a = calculateArea plot
    let p = calculatePerimeter garden plot 
    a * p

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let garden = Garden.fromList lines 
    let foo = fillPlot garden (0, 0)
    let plots = findPlots garden 
    // plots |> List.length |> printfn "%d"
    // plots |> List.map (fenceCost garden) |> printfn "%A"
    plots |> List.sumBy (fenceCost garden) |> printfn "%A"

run "input"
