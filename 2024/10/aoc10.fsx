// Advent of Code 2024. Day 10: Hoof It.
// dotnet fsi aoc10.fsx

open System
open System.IO

module Array2D = 
    let inBounds (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        first && second
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

let findTrailheads board = 
    board |> Array2D.positions |> List.choose (fun (x, y) -> if 0 = (Array2D.get board y x) then Some (x, y) else None)

let findNext board height (x, y) = 
    [ (x, y - 1); (x - 1, y); (x, y + 1); (x + 1, y) ]
    |> List.filter (Array2D.inBounds board)
    |> List.choose (fun (x, y) -> if (height + 1) = (Array2D.get board y x) then Some (x, y) else None)

let findScore board pos = 
    let rec find height pos = 
        if height = 9 then [ pos ]
        else pos |> findNext board height |> List.collect (fun (x, y) -> find (height + 1) (x, y))
    pos |> find 0 |> Set.ofList |> Set.count

let findRating board pos = 
    let rec find height pos = 
        if height = 9 then 1
        else pos |> findNext board height |> List.sumBy (fun (x, y) -> find (height + 1) (x, y))
    pos |> find 0 

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let board = Array2D.fromList lines |> Array2D.map (fun ch -> int (ch - '0'))
    let trailheads = board |> findTrailheads
    trailheads |> List.sumBy (findScore board) |> printfn "%d"
    trailheads |> List.sumBy (findRating board) |> printfn "%d"

run "input.txt"
