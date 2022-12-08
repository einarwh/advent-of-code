
open System.IO

let parseRow (line : string): int array = 
    line |> Seq.map (fun c -> (int c) - (int '0')) |> Seq.toArray

let isVisibleNorth (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    seq { 0 .. (y - 1) } |> Seq.forall (fun i -> t > Array2D.get grid x i)

let isVisibleSouth (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    let yMax = Array2D.length1 grid 
    seq { y + 1 .. yMax - 1 } |> Seq.forall (fun i -> t > Array2D.get grid x i)

let isVisibleEast (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    seq { 0 .. x - 1 } |> Seq.forall (fun i -> t > Array2D.get grid i y)

let isVisibleWest (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
    let xMax = Array2D.length2 grid 
    seq { x + 1 .. xMax - 1 } |> Seq.forall (fun i -> t > Array2D.get grid i y)

let isVisible (grid : int[,]) (x : int) (y : int) (t : int) : bool = 
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

let getScenicScore (grid : int[,]) (x : int) (y : int) (t : int) : int =
    0

let part2 (grid : int[,]) = 
    grid 
    |> Array2D.mapi (getScenicScore grid) 
    |> Seq.cast<int>
    |> Seq.max 
    |> printfn "Scenic max: %d"

let run (grid : int[,]) = 
    grid |> part1

"input"
|> File.ReadAllLines
|> Array.map parseRow 
|> array2D
|> run 
