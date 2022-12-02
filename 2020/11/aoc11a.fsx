// Advent of Code 2020. Day 11, Part A.
// dotnet fsi aoc11a.fsx

open System.IO

type Seat = | Empty | Occupied

type Position = Seat option 

type Row = Position array 

type Layout = Row array

let readPos (c : char) : Position =
    match c with
    | 'L' -> Some Empty
    | '#' -> Some Occupied
    | _ -> None

let readRow (line : string) : Row =
    line |> Seq.map readPos |> Seq.toArray

let readLayout (lines : string array) : Layout =
    lines
    |> Array.filter (fun s -> s.Length > 1)
    |> Array.map readRow

let rowCount (layout : Layout) = layout |> Array.length 

let colCount (layout : Layout) = layout |> Array.item 0 |> Array.length 

let lookup (layout : Layout) (rowNo : int) (colNo : int) : Position =
    if rowNo >= 0 && rowNo < rowCount layout then
        let row = layout |> Array.item rowNo
        if colNo >= 0 && colNo < colCount layout then
            row |> Array.item colNo
        else
            None
    else None

let findNeighborPositions (layout : Layout) (rowNo : int) (colNo : int) : Position array =
    [|
        lookup layout (rowNo-1) (colNo-1) 
        lookup layout (rowNo-1) (colNo) 
        lookup layout (rowNo-1) (colNo+1) 
        lookup layout (rowNo) (colNo-1) 
        lookup layout (rowNo) (colNo+1) 
        lookup layout (rowNo+1) (colNo-1) 
        lookup layout (rowNo+1) (colNo) 
        lookup layout (rowNo+1) (colNo+1) 
    |]

let findNeighborSeats (layout : Layout) (rowNo : int) (colNo : int) : Seat array =
    findNeighborPositions layout rowNo colNo |> Array.choose id

let countOccupiedSeats (seats : Seat array) : int =
    seats
    |> Array.map (fun s -> match s with | Empty -> 0 | Occupied -> 1)
    |> Array.sum
    
let countOccupiedNeighborSeats (layout : Layout) (rowNo : int) (colNo : int) : int =
    findNeighborSeats layout rowNo colNo
    |> countOccupiedSeats

let countOccupiedRow (row : Row) : int =
    row
    |> Array.choose id
    |> countOccupiedSeats
    
let countOccupiedTotal (layout : Layout) : int =
    layout
    |> Array.map countOccupiedRow
    |> Array.sum

let evolveSeat (layout : Layout) (rowNo : int) (colNo : int) (seat : Seat) : Seat =
    let occupied = countOccupiedNeighborSeats layout rowNo colNo
    match seat with
    | Empty -> if occupied = 0 then Occupied else Empty
    | Occupied -> if occupied >= 4 then Empty else Occupied

let evolvePosition (layout : Layout) (rowNo : int) (colNo : int) (pos : Position) : Position =
    pos |> Option.map (evolveSeat layout rowNo colNo) 

let evolveRow (layout : Layout) (rowNo : int) (row : Row) : Row =
    row
    |> Array.mapi (evolvePosition layout rowNo)

let evolveLayout (layout : Layout) : Layout =
    layout
    |> Array.mapi (evolveRow layout)

let evolve (layout : Layout) : Layout =
    let rec loop prevLayout currentLayout =
        if currentLayout = prevLayout then
            currentLayout
        else
            loop currentLayout (evolveLayout currentLayout)
    loop layout (evolveLayout layout)

let run lines =
    let layout = readLayout lines
    let layout' = evolve layout
    let count = countOccupiedTotal layout'
    printfn "count: %d" count

"input" |> File.ReadAllLines |> run 