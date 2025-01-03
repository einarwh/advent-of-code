// Advent of Code 2024. Day 04: Ceres Search.
// dotnet fsi aoc04.fsx

open System
open System.IO

module Array2D = 
    let tryGet (a : 'a[,]) (x, y) = 
        let first = y >= 0 && y < a.GetLength(0)
        let second = x >= 0 && x < a.GetLength(1)
        if first && second then Some (Array2D.get a y x) else None

let findXmas board xpos move = 
    let mpos = move xpos
    let apos = move mpos 
    let spos = move apos 
    let word = 
        [ Array2D.tryGet board xpos  
          Array2D.tryGet board mpos 
          Array2D.tryGet board apos 
          Array2D.tryGet board spos ]
        |> List.choose id
    if word = ['X'; 'M'; 'A'; 'S'] then 1 else 0

let findXmasAllAround board pos = 
    let nn = findXmas board pos (fun (x, y) -> (x, y-1))
    let nw = findXmas board pos (fun (x, y) -> (x-1, y-1))
    let ww = findXmas board pos (fun (x, y) -> (x-1, y))
    let sw = findXmas board pos (fun (x, y) -> (x-1, y+1))
    let ss = findXmas board pos (fun (x, y) -> (x, y+1))
    let se = findXmas board pos (fun (x, y) -> (x+1, y+1))
    let ee = findXmas board pos (fun (x, y) -> (x+1, y))
    let ne = findXmas board pos (fun (x, y) -> (x+1, y-1))
    [nn; nw; ww; sw; ss; se; ee; ne] |> List.sum

let findMasX board (x, y) = 
    let nwse = [ Array2D.tryGet board (x-1, y+1)  
                 Array2D.tryGet board (x, y) 
                 Array2D.tryGet board (x+1, y-1) ] |> List.choose id 
    let swne = [ Array2D.tryGet board (x-1, y-1)  
                 Array2D.tryGet board (x, y) 
                 Array2D.tryGet board (x+1, y+1) ] |> List.choose id 
    let isMas word = word = ['M'; 'A'; 'S'] || word = ['S'; 'A'; 'M']
    if isMas nwse && isMas swne then 1 else 0 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let width = lines |> List.head |> Seq.length 
    let height = lines |> List.length 
    let board = Array2D.init height width (fun y x -> lines.[y].[x])
    let positions = [for x in [0..width-1] do for y in [0..height-1] -> (x, y)]
    // Part 1
    positions 
    |> List.sumBy (findXmasAllAround board)
    |> printfn "%d"
    // Part 2
    positions 
    |> List.sumBy (findMasX board)
    |> printfn "%d"

run "input"
