// Advent of Code 2015. Day 06: Probably a Fire Hazard.
// dotnet fsi aoc06.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Pos = int * int

type Rect = Pos * Pos

type Instruction = 
    | Toggle of Rect
    | TurnOn of Rect
    | TurnOff of Rect

module Grid = 
    let create width height = 
        Array2D.create height width false
    let width grid = 
        Array2D.length2 grid
    let height grid = 
        Array2D.length1 grid
    let get (grid : bool[,]) (x, y) =
        Array2D.get grid y x
    let set (grid : bool[,]) (x, y) (value : bool) =
        Array2D.set grid y x value
    let toggle (grid : bool[,]) (rect : Rect) =
        grid 
    let turnOn (grid : bool[,]) (rect : Rect) =
        grid 
    let turnOff (grid : bool[,]) (rect : Rect) =
        grid 
    let countLit (grid : bool[,]) = 
        let w = width grid
        let h = height grid
        let posList = [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        posList |> List.filter (fun pos -> get grid pos) |> List.length

let tryParseRect (s : string) : Rect option =
    let m = Regex.Match(s, "(\d+),(\d+) through (\d+),(\d+)$")
    if m.Success then
        let read (index : int) = m.Groups.[index].Value |> int
        Some ((read 1, read 2), (read 3, read 4))
    else
        None

let tryParseToggle (s : string) : Instruction option =
    if s.StartsWith "toggle" then 
        s |> tryParseRect |> Option.map Toggle
    else 
        None

let tryParseTurnOn (s : string) : Instruction option =
    if s.StartsWith "turn on" then 
        s |> tryParseRect |> Option.map TurnOn
    else 
        None

let tryParseTurnOff (s : string) : Instruction option =
    if s.StartsWith "turn off" then 
        s |> tryParseRect |> Option.map TurnOff
    else 
        None

let tryParse (s : string) = 
    tryParseToggle s
    |> Option.orElse (tryParseTurnOn s)
    |> Option.orElse (tryParseTurnOff s)

let getRectPositions (rect : Rect) = 
    let ((xStart, yStart), (xEnd, yEnd)) = rect 
    let xs = [xStart .. xEnd]
    let ys = [yStart .. yEnd]
    [for x in xs do for y in ys -> (x, y)]

let toggle (grid : bool[,]) (rect : Rect) = 
    let rec loop posList = 
        match posList with
        | [] -> ()
        | pos :: rest -> 
            let v = Grid.get grid pos
            Grid.set grid pos (not v)
            loop rest 
    rect |> getRectPositions |> loop

let turnOn (grid : bool[,]) (rect : Rect) = 
    let rec loop posList = 
        match posList with
        | [] -> ()
        | pos :: rest -> 
            Grid.set grid pos true
            loop rest 
    rect |> getRectPositions |> loop

let turnOff (grid : bool[,]) (rect : Rect) = 
    let rec loop posList = 
        match posList with
        | [] -> ()
        | pos :: rest -> 
            Grid.set grid pos false
            loop rest 
    rect |> getRectPositions |> loop

let execute grid (instructions : Instruction list) = 
    let rec loop instList = 
        match instList with 
        | [] -> ()
        | inst :: rest -> 
            match inst with 
            | Toggle rect -> toggle grid rect 
            | TurnOn rect -> turnOn grid rect 
            | TurnOff rect -> turnOff grid rect
            rest |> loop
    instructions |> loop

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let visualize grid = 
    let w = Grid.width grid 
    let h = Grid.height grid
    let createRow y = 
        [ 0 .. (w - 1) ] 
        |> List.map (fun x -> if Grid.get grid (x, y) then "#" else ".")
        |> String.concat ""
    [ 0 .. (h - 1) ] 
    |> List.map createRow |> String.concat "\n" |> printfn "%s"
    printfn ""

let run fileName = 
    let lines = readLines fileName
    let instructions = lines |> List.choose tryParse
    // instructions |> printfn "%A"
    let grid = Grid.create 1000 1000
    execute grid instructions
    grid |> Grid.countLit |> printfn "%d"
    // visualize grid

run "input"
