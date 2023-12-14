// Advent of Code 2023. Day 14: Parabolic Reflector Dish
// dotnet fsi aoc14.fsx

open System
open System.IO

module Array2D = 

    let getRowCount (ary : 'a[,]) = ary.GetLength(0)

    let getColumnCount (ary : 'a[,]) = ary.GetLength(1)

    let getRow index (ary : 'a[,]) = ary[index, *]

    let getColumn index (ary : 'a[,]) = ary[*, index]

    let getRows (ary : 'a[,]) = 
        [|0 .. getRowCount ary - 1|]
        |> Array.map (fun ix -> getRow ix ary) 

    let getColumns (ary : 'a[,]) = 
        [|0 .. getColumnCount ary - 1|]
        |> Array.map (fun ix -> getColumn ix ary) 

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let rec toIntervals (chars : char array) = 
    let indexes = 
        chars 
        |> Array.toList 
        |> List.indexed 
        |> List.choose (fun (i, ch) -> if ch = '#' then Some i else None)
    let len = Array.length chars
    let rec loop pos indexes = 
        match indexes with 
        | [] -> 
            [ (pos, len - 1)]
        | ix :: rest -> 
            (pos, ix - 1) :: loop (ix + 1) rest
    indexes |> loop 0 |> List.filter (fun (a, b) -> a <= b)

let rec tiltRowEast (intervals : (int * int) list) (rowIndex : int) (ary : char[,]) = 
    match intervals with 
    | [] -> ()
    | (start, stop) :: rest ->
        let range = [start .. stop]
        let rocks = 
            range 
            |> List.map (fun colIndex -> Array2D.get ary rowIndex colIndex) 
            |> List.filter ((=) 'O')
            |> List.length
        let spaces = List.length range - rocks
        [ start .. start + spaces - 1 ]
        |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex '.')
        [ start + spaces .. stop ] 
        |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex 'O')
        tiltRowEast rest rowIndex ary

let tiltEast rowIntervals ary = 
    let rowCount = Array2D.getRowCount ary
    [ 0 .. rowCount - 1 ]
    |> List.iter (fun rowIndex -> tiltRowEast (Array.get rowIntervals rowIndex) rowIndex ary)

let rec tiltRowWest (intervals : (int * int) list) (rowIndex : int) (ary : char[,]) = 
    match intervals with 
    | [] -> 
        ()
    | (start, stop) :: rest ->
        let range = [start .. stop]
        let rocks = 
            range 
            |> List.map (fun colIndex -> Array2D.get ary rowIndex colIndex) 
            |> List.filter ((=) 'O')
            |> List.length
        let rockRange = [ start .. start + rocks - 1 ]
        let spaceRange = [ start + rocks .. stop ]
        rockRange
        |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex 'O')
        spaceRange 
        |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex '.')
        tiltRowWest rest rowIndex ary

let tiltWest rowIntervals ary = 
    let rowCount = Array2D.getRowCount ary
    [ 0 .. rowCount - 1 ]
    |> List.iter (fun rowIndex -> tiltRowWest (Array.get rowIntervals rowIndex) rowIndex ary)

let rec tiltColumnNorth (intervals : (int * int) list) (colIndex : int) (ary : char[,]) = 
    match intervals with 
    | [] -> ()
    | (start, stop) :: rest ->
        let range = [start .. stop]
        let rocks = 
            range 
            |> List.map (fun rowIndex -> Array2D.get ary rowIndex colIndex) 
            |> List.filter ((=) 'O')
            |> List.length
        let rockRange = [ start .. start + rocks - 1 ]
        let spaceRange = [ start + rocks .. stop ]
        rockRange
        |> List.iter (fun rowIndex -> Array2D.set ary rowIndex colIndex 'O')
        spaceRange 
        |> List.iter (fun rowIndex -> Array2D.set ary rowIndex colIndex '.')
        tiltColumnNorth rest colIndex ary
let tiltNorth colIntervals ary = 
    let colCount = Array2D.getColumnCount ary
    [ 0 .. colCount - 1 ]
    |> List.iter (fun colIndex -> tiltColumnNorth (Array.get colIntervals colIndex) colIndex ary)

let rec tiltColumnSouth (intervals : (int * int) list) (colIndex : int) (ary : char[,]) = 
    match intervals with 
    | [] -> ()
    | (start, stop) :: rest ->

// let spaces = List.length range - rocks
//         [ start .. start + spaces - 1 ]
//         |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex '.')
//         [ start + spaces .. stop ] 
//         |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex 'O')
//         tiltRowEast rest rowIndex ary

        let range = [start .. stop]
        let rocks = 
            range 
            |> List.map (fun rowIndex -> Array2D.get ary rowIndex colIndex) 
            |> List.filter ((=) 'O')
            |> List.length
        let spaces = List.length range - rocks
        let spaceRange = [ start .. start + spaces - 1 ]
        let rockRange = [ start + spaces .. stop ]
        rockRange
        |> List.iter (fun rowIndex -> Array2D.set ary rowIndex colIndex 'O')
        spaceRange 
        |> List.iter (fun rowIndex -> Array2D.set ary rowIndex colIndex '.')
        tiltColumnSouth rest colIndex ary

let tiltSouth colIntervals ary = 
    let colCount = Array2D.getColumnCount ary
    [ 0 .. colCount - 1 ]
    |> List.iter (fun colIndex -> tiltColumnSouth (Array.get colIntervals colIndex) colIndex ary)

let cycle rowIntervals colIntervals ary = 
    tiltNorth colIntervals ary
    tiltWest rowIntervals ary
    tiltSouth colIntervals ary 
    tiltEast rowIntervals ary

let calculateLoad lines = 
    let countRocks = Seq.filter ((=) 'O') >> Seq.length 
    let len = lines |> List.length 
    lines |> List.mapi (fun i line -> (len - i) * countRocks line) |> List.sum

let solve limit cycleFn (ary : char[,]) = 
    let rec loop n seen (current : char [,]) = 
        if n > limit then 
            None
        else 
            if List.contains current seen then 
                let interval = 1 + List.findIndex ((=) current) seen 
                let preceding = List.length seen - interval
                let sequence = seen |> List.rev |> List.skip preceding
                let ix = (limit - preceding) % (interval)
                let chosen = sequence |> List.item ix 
                let rows = chosen |> Array2D.getRows |> Array.toList
                let load = rows |> calculateLoad
                Some load
            else 
                let copy : char[,] = Array2D.copy current 
                cycleFn copy 
                loop (n + 1) (current :: seen) copy
    loop 0 [] ary 

let run fileName =
    let lines = readLines fileName |> Array.toList
    let tarr = array2D [[0;1;3]; [4;5;6]] 
    Array2D.length1 tarr |> printfn "length1 = %A" // row count (2)
    Array2D.length2 tarr |> printfn "length2 = %A" // col count (3)
    let ary = array2D lines
    Array2D.getRow 1 tarr |> printfn "row 1 %A"
    Array2D.getColumn 1 tarr |> printfn "column 1 %A"

    let rowIntervals = 
        [|0 .. Array2D.getRowCount ary - 1|]
        |> Array.map (fun i -> Array2D.getRow i ary)
        |> Array.map (fun row -> toIntervals row)
    rowIntervals |> Array.iter (printfn "%A")
    let colIntervals = 
        [|0 .. Array2D.getColumnCount ary - 1|]
        |> Array.map (fun i -> Array2D.getColumn i ary)
        |> Array.map (fun col -> toIntervals col)

    // printfn "before tilt"
    // [|0 .. Array2D.getRowCount ary - 1|]
    // |> Array.map (fun i -> Array2D.getRow i ary)
    // |> Array.iter (printfn "%A")
    // printfn ".."

    // tiltSouth colIntervals ary
    // printfn "after tilt south"
    // [|0 .. Array2D.getRowCount ary - 1|]
    // |> Array.map (fun i -> Array2D.getRow i ary)
    // |> Array.iter (printfn "%A")

    let cycleFn = cycle rowIntervals colIntervals

    // cycleFn ary
    // cycleFn ary
    // cycleFn ary
    [|0 .. Array2D.getRowCount ary - 1|]
    |> Array.map (fun i -> Array2D.getRow i ary)
    |> Array.iter (printfn "%A")
    printfn "......"
    Array2D.getRows ary
    |> Array.iter (printfn "%A")

    let limit = 1000000000
    match solve limit cycleFn ary with 
    | None -> printfn "?"
    | Some load -> printfn "%d" load 
    
"sample" |> run
