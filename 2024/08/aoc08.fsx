// Advent of Code 2024. Day 08
// dotnet fsi aoc08.fsx

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

let findAntinodes board ((x1, y1), (x2, y2)) = 
    let xd = x2 - x1 
    let yd = y2 - y1
    [ (x1 - xd, y1 - yd); (x2 + xd, y2 + yd) ] |> List.filter (Array2D.inBounds board)

let findAntinodesWithHarmonics board ((x1, y1), (x2, y2)) = 
    let xd = x2 - x1 
    let yd = y2 - y1
    let sub (x, y) = (x - xd, y - yd) 
    let add (x, y) = (x + xd, y + yd) 
    let unfold fn = List.unfold (fun pos -> if Array2D.inBounds board pos then Some (pos, fn pos) else None)
    let subList = (x1, y1) |> unfold sub
    let addList = (x2, y2) |> unfold add
    subList @ addList 

let rec pairs lst = 
    match lst with 
    | [] -> []
    | h :: t -> 
        List.map (fun it -> (h, it)) t @ pairs t 

let countAntinodes antinodeFinder board = 
    let check board (x, y) = 
        match Array2D.get board y x with 
        | '.' -> None 
        | antenna -> Some (antenna, (x, y))
    let antennae = 
        board 
        |> Array2D.positions 
        |> List.choose (check board) 
        |> List.groupBy (fun (a, p)-> a)
        |> List.map (fun (a, lst) -> (a, List.map snd lst))
    antennae
    |> List.map (fun (a, positions) -> (positions |> pairs |> List.collect (antinodeFinder board)))
    |> List.concat
    |> Set.ofList 
    |> Set.count 

let run fileName = 
    let lines = readLines fileName |> List.map Seq.toList
    let board = Array2D.fromList lines
    board |> countAntinodes findAntinodes |> printfn "%d"
    board |> countAntinodes findAntinodesWithHarmonics |> printfn "%d"

run "sample"
