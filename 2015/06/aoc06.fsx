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

module Screen = 
    let create width height = 
        Array2D.create height width false
    let width screen = 
        Array2D.length2 screen
    let height screen = 
        Array2D.length1 screen
    let get (screen : bool[,]) (x, y) =
        Array2D.get screen y x
    let set (screen : bool[,]) (x, y) (value : bool) =
        Array2D.set screen y x value
    let toggle (screen : bool[,]) (rect : Rect) =
        screen 
    let turnOn (screen : bool[,]) (rect : Rect) =
        screen 
    let turnOff (screen : bool[,]) (rect : Rect) =
        screen 
    let countLit (screen : bool[,]) = 
        let w = width screen
        let h = height screen
        let posList = [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        posList |> List.filter (fun pos -> get screen pos) |> List.length


let tryParseToggle (s : string) : Instruction option =
    let m = Regex.Match(s, "^toggle (\d+),(\d+) through (\d+),(\d+)$")
    if m.Success then
        let read (index : int) = m.Groups.[index].Value |> int
        Some <| Toggle ((read 1, read 2), (read 3, read 4))
    else
        None

let tryParseTurnOn (s : string) : Instruction option =
    let m = Regex.Match(s, "^turn on (\d+),(\d+) through (\d+),(\d+)$")
    if m.Success then
        let read (index : int) = m.Groups.[index].Value |> int
        Some <| TurnOn ((read 1, read 2), (read 3, read 4))
    else
        None

let tryParseTurnOff (s : string) : Instruction option =
    let m = Regex.Match(s, "^turn off (\d+),(\d+) through (\d+),(\d+)$")
    if m.Success then
        let read (index : int) = m.Groups.[index].Value |> int
        Some <| TurnOn ((read 1, read 2), (read 3, read 4))
    else
        None

let tryParse (s : string) = 
    tryParseToggle s
    |> Option.orElse (tryParseTurnOn s)
    |> Option.orElse (tryParseTurnOff s)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let instructions = lines |> List.choose tryParse
    instructions |> printfn "%A"

run "input"
