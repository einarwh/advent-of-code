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

let rec toIntervals chars = 
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

let rec tiltRowEast intervals rowIndex ary = 
    match intervals with 
    | [] -> ()
    | (start, stop) :: rest ->
        let range = [start .. stop]
        let spaces = 
            range 
            |> List.map (fun colIndex -> Array2D.get ary rowIndex colIndex) 
            |> List.filter ((=) '.')
            |> List.length
        let spaceRange = [ start .. start + spaces - 1 ]
        let rockRange = [ start + spaces .. stop ]
        spaceRange
        |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex '.')
        rockRange
        |> List.iter (fun colIndex -> Array2D.set ary rowIndex colIndex 'O')
        tiltRowEast rest rowIndex ary

let tiltEast rowIntervals ary = 
    let rowCount = Array2D.getRowCount ary
    [ 0 .. rowCount - 1 ]
    |> List.iter (fun rowIndex -> tiltRowEast (Array.get rowIntervals rowIndex) rowIndex ary)

let rec tiltRowWest intervals rowIndex ary = 
    match intervals with 
    | [] -> ()
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

let rec tiltColumnNorth intervals colIndex ary = 
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

let rec tiltColumnSouth intervals colIndex ary = 
    match intervals with 
    | [] -> ()
    | (start, stop) :: rest ->
        let range = [start .. stop]
        let spaces = 
            range 
            |> List.map (fun rowIndex -> Array2D.get ary rowIndex colIndex) 
            |> List.filter ((=) '.')
            |> List.length
        let spaceRange = [ start .. start + spaces - 1 ]
        let rockRange = [ start + spaces .. stop ]
        spaceRange 
        |> List.iter (fun rowIndex -> Array2D.set ary rowIndex colIndex '.')
        rockRange
        |> List.iter (fun rowIndex -> Array2D.set ary rowIndex colIndex 'O')
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
    let ary = array2D lines
    let rowIntervals = Array2D.getRows ary |> Array.map toIntervals
    let colIntervals = Array2D.getColumns ary |> Array.map toIntervals
    let tilted = Array2D.copy ary 
    tiltNorth colIntervals tilted 
    tilted |> Array2D.getRows |> Array.toList |> calculateLoad |> printfn "%d"
    let cycleFn = cycle rowIntervals colIntervals
    let limit = 1000000000
    match solve limit cycleFn (Array2D.copy ary) with 
    | None -> printfn "?"
    | Some load -> printfn "%d" load 
    
"input.txt" |> run
