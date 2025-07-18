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
        Array2D.create height width 0
    let width grid = 
        Array2D.length2 grid
    let height grid = 
        Array2D.length1 grid
    let get (grid : int[,]) (x, y) =
        Array2D.get grid y x
    let set (grid : int[,]) (x, y) (value : int) =
        Array2D.set grid y x value
    let count (grid : int[,]) = 
        let w = width grid
        let h = height grid
        let posList = [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        posList |> List.map (fun pos -> get grid pos) |> List.sum
    let brightest (grid : int[,]) = 
        let w = width grid
        let h = height grid
        let posList = [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        posList |> List.map (fun pos -> get grid pos) |> List.max

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

let update (fn : int -> int) (grid : int[,]) (rect : Rect) = 
    let rec loop posList = 
        match posList with
        | [] -> ()
        | pos :: rest -> 
            let v = Grid.get grid pos
            Grid.set grid pos (fn v)
            loop rest 
    rect |> getRectPositions |> loop

let execute toggle turnOn turnOff grid (instructions : Instruction list) = 
    let rec loop instList = 
        match instList with 
        | [] -> ()
        | inst :: rest -> 
            match inst with 
            | Toggle rect -> update toggle grid rect 
            | TurnOn rect -> update turnOn grid rect 
            | TurnOff rect -> update turnOff grid rect
            rest |> loop
    instructions |> loop

let execute1 = execute (fun v -> if v = 0 then 1 else 0) (fun _ -> 1) (fun _ -> 0)

let execute2 = execute (fun v -> v + 2) (fun v -> v + 1) (fun v -> max 0 (v - 1))

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let instructions = lines |> List.choose tryParse
    let grid1 = Grid.create 1000 1000
    execute1 grid1 instructions
    grid1 |> Grid.count |> printfn "%d"
    let grid2 = Grid.create 1000 1000
    execute2 grid2 instructions
    grid2 |> Grid.count |> printfn "%d"
    // grid2 |> Grid.brightest |> printfn "%d"

run "input"
