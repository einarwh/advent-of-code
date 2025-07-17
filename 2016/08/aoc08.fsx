// Advent of Code 2016. Day 08
// dotnet fsi aoc08.fsx

open System
open System.IO

type Operation = 
    | Rectangle of int*int 
    | RotateRow of int*int
    | RotateColumn of int*int

module Screen = 
    let create width height = 
        Array2D.create height width false
    let width screen = 
        Array2D.length2 screen
    let height screen = 
        Array2D.length1 screen
    let get (screen : bool[,]) (x, y) =
        Array2D.get screen y x

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let visualize screen = 
    let w = Screen.width screen 
    let h = Screen.height screen
    printfn "width %d" w 
    printfn "height %d" h 
    let createRow y = 
        printfn "row: %d" y
        [ 0 .. (w - 1) ] 
        |> List.map (fun x -> if Screen.get screen (x, y) then "#" else ".")
        |> String.concat ""
    [ 0 .. (h - 1) ] 
    |> List.map createRow |> String.concat "\n" |> printfn "%s"

let run fileName = 
    let lines = readLines fileName
    lines |> printfn "%A"
    let screen = Screen.create 50 6 
    printfn "screen %A" screen
    visualize screen
    

run "sample"
