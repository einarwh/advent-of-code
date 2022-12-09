// Advent of Code 2022. 
// Day 9: Rope Bridge.
// dotnet fsi aoc09.fsx

open System.IO
open System.Text.RegularExpressions

type Direction = N | W | S | E

type Motion = (Direction * int) 

type Pos = (int * int)

let toDirection s =  
    match s with 
    | "U" -> N 
    | "L" -> W
    | "D" -> S
    | "R" -> E
    | _ -> failwith <| sprintf "invalid direction %s" s

let toDirections motion = 
    match motion with 
    | (d, s) -> [1 .. s] |> List.map (fun _ -> d)

let tryParseMotion (s : string) : Motion option = 
    let pattern = "^(U|L|D|R) (\d+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let dir = m.Groups.[1].Value |> toDirection
        let steps = m.Groups.[2].Value |> int
        Some (dir, steps)
    else 
        None 

let updateHead (d : Direction) (x : int, y : int) : Pos = 
    match d with 
    | N -> (x, y+1)
    | W -> (x-1, y)
    | S -> (x, y-1)
    | E -> (x+1, y)

let toTailMove (hx, hy) (tx, ty) : Pos = 
    match (hx - tx, hy - ty) with 
    | (-1,  2) -> (-1,  1)
    | ( 0,  2) -> ( 0,  1)
    | ( 1,  2) -> ( 1,  1)
    | ( 2,  1) -> ( 1,  1)
    | ( 2,  0) -> ( 1,  0)
    | ( 2, -1) -> ( 1, -1)
    | ( 1, -2) -> ( 1, -1)
    | ( 0, -2) -> ( 0, -1)
    | (-1, -2) -> (-1, -1)
    | (-2, -1) -> (-1, -1)
    | (-2,  0) -> (-1,  0)
    | (-2,  1) -> (-1,  1)
    | _ -> (0, 0)

let updateTail (hx, hy) (tx, ty) : Pos = 
    match toTailMove (hx, hy) (tx, ty)  with 
    | (dx, dy) -> (tx + dx, ty + dy)

let part1 (dirs : Direction list) = 
    let rec fn (headPos : Pos) (tailPos : Pos) (visited : Set<Pos>) (dirs : Direction list) : Set<Pos> = 
        match dirs with 
        | [] -> 
            visited
        | d :: rest -> 
            let (headPos' : Pos) = headPos |> updateHead d
            let (tailPos' : Pos) = tailPos |> updateTail headPos' 
            let (visited' : Set<Pos>) = visited |> Set.add tailPos'
            fn headPos' tailPos' visited' rest 
    let startPos = (0, 0)
    let positions = fn startPos startPos Set.empty dirs
    positions |> Set.count

let run motions = 
    motions 
    |> List.collect toDirections
    |> part1 
    |> printfn "Visited: %d"

"input"
|> File.ReadAllLines
|> Array.toList 
|> List.choose tryParseMotion
|> run 