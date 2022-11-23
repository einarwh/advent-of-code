open System.IO

type Seat = | Empty | Occupied
type Position = Seat option
type Lookup = Position option 
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

let check (layout : Layout) (rowNo : int, colNo : int) : Position option =
    if rowNo >= 0 && rowNo < rowCount layout then
        let row = layout |> Array.item rowNo
        if colNo >= 0 && colNo < colCount layout then
            row |> Array.item colNo |> Some 
        else
            None
    else None

let lookAt (layout : Layout) (rowNo : int, colNo : int) : Position =
    check layout (rowNo, colNo) |> Option.defaultValue None

let rec lookDir (dir : (int * int) -> (int * int)) (layout : Layout) (xy : (int * int)) : Position =
    let xy' = dir xy
    match check layout xy' with
    | None -> None
    | Some pos -> pos |> Option.orElse (lookDir dir layout xy')

let findDirectNeighborPositions (layout : Layout) (rowNo : int) (colNo : int) : Position array =
    [|
        lookAt layout (rowNo-1, colNo-1) 
        lookAt layout (rowNo-1, colNo) 
        lookAt layout (rowNo-1, colNo+1) 
        lookAt layout (rowNo, colNo-1) 
        lookAt layout (rowNo, colNo+1) 
        lookAt layout (rowNo+1, colNo-1) 
        lookAt layout (rowNo+1, colNo) 
        lookAt layout (rowNo+1, colNo+1) 
    |]

let findPositionsInDirection (layout : Layout) (rowNo : int) (colNo : int) : Position array =
    [|
        lookDir (fun (r, c) -> (r-1, c-1)) layout (rowNo, colNo) 
        lookDir (fun (r, c) -> (r-1, c)) layout (rowNo, colNo) 
        lookDir (fun (r, c) -> (r-1, c+1)) layout (rowNo, colNo) 
        lookDir (fun (r, c) -> (r, c-1)) layout (rowNo, colNo) 
        lookDir (fun (r, c) -> (r, c+1)) layout (rowNo, colNo) 
        lookDir (fun (r, c) -> (r+1, c-1)) layout (rowNo, colNo) 
        lookDir (fun (r, c) -> (r+1, c)) layout (rowNo, colNo) 
        lookDir (fun (r, c) -> (r+1, c+1)) layout (rowNo, colNo) 
    |]
    
let findDirectNeighborSeats (layout : Layout) (rowNo : int) (colNo : int) : Seat array =
    findDirectNeighborPositions layout rowNo colNo |> Array.choose id

let countOccupiedSeats (seats : Seat array) : int =
    seats
    |> Array.map (fun s -> match s with | Empty -> 0 | Occupied -> 1)
    |> Array.sum
    
let countOccupiedDirectNeighborSeats (layout : Layout) (rowNo : int) (colNo : int) : int =
    findDirectNeighborSeats layout rowNo colNo
    |> countOccupiedSeats

let findSeatsInDirection (layout : Layout) (rowNo : int) (colNo : int) : Seat array =
    findPositionsInDirection layout rowNo colNo |> Array.choose id

let countOccupiedSeatsInDirection (layout : Layout) (rowNo : int) (colNo : int) : int =
    findSeatsInDirection layout rowNo colNo
    |> countOccupiedSeats

let countOccupiedRow (row : Row) : int =
    row
    |> Array.choose id
    |> countOccupiedSeats
    
let countOccupiedTotal (layout : Layout) : int =
    layout
    |> Array.map countOccupiedRow
    |> Array.sum

let evolveSeatDirectNeighbors (layout : Layout) (rowNo : int) (colNo : int) (seat : Seat) : Seat =
    let occupied = countOccupiedDirectNeighborSeats layout rowNo colNo
    match seat with
    | Empty -> if occupied = 0 then Occupied else Empty
    | Occupied -> if occupied >= 4 then Empty else Occupied

let evolveSeatInDirection (layout : Layout) (rowNo : int) (colNo : int) (seat : Seat) : Seat =
    let occupied = countOccupiedSeatsInDirection layout rowNo colNo
    match seat with
    | Empty -> if occupied = 0 then Occupied else Empty
    | Occupied -> if occupied >= 5 then Empty else Occupied
    
let evolvePosition (evolveSeat : Layout -> int -> int -> Seat -> Seat) (layout : Layout)  (rowNo : int) (colNo : int) (pos : Position) : Position =
    pos |> Option.map (evolveSeat layout rowNo colNo) 

let evolveRow (evolveSeat : Layout -> int -> int -> Seat -> Seat) (layout : Layout) (rowNo : int) (row : Row)  : Row =
    row
    |> Array.mapi (evolvePosition evolveSeat layout rowNo)

let evolveLayout (evolveSeat : Layout -> int -> int -> Seat -> Seat) (layout : Layout) : Layout =
    layout
    |> Array.mapi (evolveRow evolveSeat layout)

let evolve (evolveSeat : Layout -> int -> int -> Seat -> Seat) (layout : Layout) : int =
    let rec loop prevLayout currentLayout =
        if currentLayout = prevLayout then
            currentLayout
        else
            loop currentLayout (evolveLayout evolveSeat currentLayout)
    loop layout (evolveLayout evolveSeat layout)
    |> countOccupiedTotal
    
let evolve1 = evolve evolveSeatDirectNeighbors

let evolve2 = evolve evolveSeatInDirection

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0]
    let layout = readLayout lines
    layout |> evolve1 |> printfn "%d"
    layout |> evolve2 |> printfn "%d"
    0