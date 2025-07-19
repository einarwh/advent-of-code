// Advent of Code 2016. Day 18: Like a Rogue.
// dotnet fsi aoc18.fsx

open System
open System.IO

let selectTile (tiles : char array) (i : int) : char = 
    let readTile ix = 
        if ix < 0 || ix >= tiles.Length then '.'
        else tiles[ix]
    let isTrap ch = ch = '^'
    let left = readTile (i - 1)
    let center = readTile i 
    let right = readTile (i + 1)
    let trap =  
        (isTrap left && isTrap center && not <| isTrap right) || 
        (not <| isTrap left && isTrap center && isTrap right) || 
        (isTrap left && not <| isTrap center && not <| isTrap right) || 
        (not <| isTrap left && not <| isTrap center && isTrap right)
    if trap then '^' else '.'

let getNextRow (tiles : char array) = 
    [|0 .. tiles.Length - 1|] |> Array.map (selectTile tiles)

let getRows (n : int) (row : char array) = 
    let rec fn acc row = 
        if List.length acc < n then 
            let nxt = getNextRow row 
            fn (nxt :: acc) nxt 
        else 
            acc |> List.rev
    fn [row] row

let getRowsX (n : int) (row : char array) = 
    let rec fn acc seen row = 
        if (List.length acc) % 10000 = 0 then printfn "acc %d" (List.length acc)
        if List.length acc < n then 
            let nxt = getNextRow row 
            if seen |> Set.contains nxt then 
                printfn "LOOP!"
                acc |> List.rev
            else 
                fn (nxt :: acc) (seen |> Set.add nxt) nxt 
        else 
            acc |> List.rev
    fn [row] Set.empty row

let countSafeTiles (rows : char array list) = 
    rows 
    |> List.collect (fun cs -> cs |> Array.toList)
    |> List.sumBy (fun ch -> if ch = '^' then 0 else 1)

let run rowCount fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> Seq.toArray |> getNextRow |> fun chars -> new string(chars) |> printfn "%s"
    let rows = text |> Seq.toArray |> getRowsX rowCount
    rows |> countSafeTiles |> printfn "%d"

// run 3 "sample1.txt"
// run 10 "sample2.txt"
run 40 "input.txt"
run 400000 "input.txt"
