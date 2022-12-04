// Advent of Code 2020. 
// Day 12: Rain Risk, Part B.
// dotnet fsi aoc12b.fsx

open System.IO
open System.Text.RegularExpressions

type Instruction =
    | N of int
    | S of int
    | E of int
    | W of int
    | L of int
    | R of int
    | F of int
    
type Position = (int * int)

let toRotations x =
    match x with
    | 90 -> 1
    | 180 -> 2
    | 270 -> 3
    | _ -> failwith "Awkward number of degrees %x"

let parseInstruction s =
    let m = Regex.Match(s, "(N|S|E|W|L|R|F)(\d+)")
    let letter = m.Groups.[1].Value
    let x = int m.Groups.[2].Value
    match letter with
    | "N" -> N x
    | "S" -> S x
    | "E" -> E x
    | "W" -> W x
    | "L" -> L <| toRotations x
    | "R" -> R <| toRotations x
    | "F" -> F x
    | _ -> failwith <| sprintf "Unknown letter %s" letter
    
let north steps (x, y) = (x, y+steps)

let west steps (x, y) = (x-steps, y)

let south steps (x, y) = (x, y-steps)

let east steps (x, y) = (x+steps, y)

let rotateLeft (x, y) = (-y, x)

let rotateRight (x, y) = (y, -x)
    
let rec times n fn =
    if n < 1 then id
    else fn >> times (n - 1) fn 
    
let left n = times n rotateLeft
    
let right n = times n rotateRight

let towards (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let forward n (ship, waypoint) =
    (ship |> times n (towards waypoint), waypoint)

let next inst (ship, waypoint) =
    match inst with
    | N x -> (ship, north x waypoint)
    | S x -> (ship, south x waypoint)
    | E x -> (ship, east x waypoint) 
    | W x -> (ship, west x waypoint)
    | L d -> (ship, left d waypoint)
    | R d -> (ship, right d waypoint)
    | F x -> forward x (ship, waypoint)

let distance (x, y) : int =
    abs x + abs y

let read =
    Array.toList
    >> List.filter (fun s -> String.length s > 0)
    >> List.map parseInstruction

let run lines =
    let start = ((0, 0), (10, 1))
    let (ship, _) =
        lines    
        |> read
        |> List.fold (fun state inst -> next inst state) start
    ship |> distance |> printfn "Manhattan distance (Part 1): %d"

"input" |> File.ReadAllLines |> run 
