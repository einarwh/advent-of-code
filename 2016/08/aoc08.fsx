// Advent of Code 2016. Day 08
// dotnet fsi aoc08.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Operation = 
    | Rect of int*int 
    | Row of int*int
    | Column of int*int

module Screen = 
    let create width height = 
        Array2D.create height width false
    let width screen = 
        Array2D.length2 screen
    let height screen = 
        Array2D.length1 screen
    let get (screen : bool[,]) (x, y) =
        Array2D.get screen y x

// let parse (s : string) : Operation = 

let tryParseRect (s : string) : Operation option = 
    let m = Regex.Match(s, "^rect (\d+)x(\d+)$")
    if m.Success then
        let w = m.Groups.[1].Value |> int 
        let h = m.Groups.[2].Value |> int 
        Some <| Rect (w, h)
    else
        None

let tryParseRow (s : string) : Operation option = 
    let m = Regex.Match(s, "^rotate row y=(\d+) by (\d+)$")
    if m.Success then
        let y = m.Groups.[1].Value |> int 
        let r = m.Groups.[2].Value |> int 
        Some <| Row (y, r)
    else
        None

let tryParseColumn (s : string) : Operation option = 
    let m = Regex.Match(s, "^rotate column x=(\d+) by (\d+)$")
    if m.Success then
        let x = m.Groups.[1].Value |> int 
        let r = m.Groups.[2].Value |> int 
        Some <| Column (x, r)
    else
        None

let tryParse (s : string) : Operation option = 
    tryParseRect s
    |> Option.orElse (tryParseRow s)
    |> Option.orElse (tryParseColumn s)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let visualize screen = 
    let w = Screen.width screen 
    let h = Screen.height screen
    let createRow y = 
        printfn "row: %d" y
        [ 0 .. (w - 1) ] 
        |> List.map (fun x -> if Screen.get screen (x, y) then "#" else ".")
        |> String.concat ""
    [ 0 .. (h - 1) ] 
    |> List.map createRow |> String.concat "\n" |> printfn "%s"

let run fileName = 
    let lines = readLines fileName
    let operations = lines |> List.choose tryParse
    operations |> printfn "%A"
    let screen = Screen.create 50 6 
    visualize screen
    

run "sample"
