// Advent of Code 2022. 
// Day 8: Treetop Tree House.
// dotnet fsi aoc08.fsx

open System.IO

let parseRow (line : string): int array = 
    line |> Seq.map (fun c -> (int c) - (int '0')) |> Seq.toArray

let isVisibleNorth (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    seq { 0 .. (y - 1) } |> Seq.forall (fun i -> t > Array2D.get grid i x)

let isVisibleSouth (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    let yMax = Array2D.length1 grid 
    seq { y + 1 .. yMax - 1 } |> Seq.forall (fun i -> t > Array2D.get grid i x)

let isVisibleEast (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    seq { 0 .. x - 1 } |> Seq.forall (fun i -> t > Array2D.get grid y i)

let isVisibleWest (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    let xMax = Array2D.length2 grid 
    seq { x + 1 .. xMax - 1 } |> Seq.forall (fun i -> t > Array2D.get grid y i)

let isVisible (grid : int[,]) (y : int) (x : int) (t : int) : bool = 
    let rowCount = Array2D.length1 grid 
    let colCount = Array2D.length2 grid 
    x = 0 
    || x = (rowCount - 1) 
    || y = 0 
    || y = (colCount - 1) 
    || isVisibleNorth grid x y t 
    || isVisibleSouth grid x y t
    || isVisibleEast grid x y t
    || isVisibleWest grid x y t

let part1 (grid : int[,]) = 
    grid 
    |> Array2D.mapi (isVisible grid) 
    |> Seq.cast<bool> 
    |> Seq.filter id
    |> Seq.length 
    |> printfn "Visible: %d"

let countVisible0 predicate trees = 
    trees 
    |> List.tryFindIndex predicate
    |> Option.defaultValue (List.length trees)

let countVisible (t : int) (selector : int -> int) (treeIndexes : int list) = 
    treeIndexes 
    |> List.tryFindIndex (fun (i : int) -> t <= selector i)
    |> Option.map ((+) 1)
    |> Option.defaultValue (List.length treeIndexes)

let countVisibleNorth grid x y t = 
    [0 .. (y - 1)] 
    |> List.rev 
    |> countVisible t (fun i -> Array2D.get grid i x)

let countVisibleSouth grid x y t = 
    let yMax = Array2D.length1 grid 
    [(y + 1) .. (yMax - 1)] |> countVisible t (fun i -> Array2D.get grid i x)

let countVisibleWest grid x y t = 
    [0 .. (x - 1)] 
    |> List.rev
    |> countVisible t (fun i -> Array2D.get grid y i)

let countVisibleEast grid x y t = 
    let xMax = Array2D.length2 grid 
    [(x + 1) .. (xMax - 1)] |> countVisible t (fun i -> Array2D.get grid y i)

let getScenicScore (grid : int[,]) (y: int) (x : int) (t : int) : int =
    let result = 
        [ countVisibleNorth grid x y t 
          countVisibleSouth grid x y t 
          countVisibleWest grid x y t 
          countVisibleEast grid x y t ]
        |> List.fold (*) 1
    result

let part2 (grid : int[,]) = 
    grid 
    |> Array2D.mapi (getScenicScore grid) 
    |> Seq.cast<int>
    |> Seq.max 
    |> printfn "Scenic max: %d"

let run (grid : int[,]) = 
    grid |> part1
    grid |> part2 

"input"
|> File.ReadAllLines
|> Array.map parseRow 
|> array2D
|> run 
