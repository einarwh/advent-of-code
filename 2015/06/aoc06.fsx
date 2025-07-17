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
