// Advent of Code 2022. 
// Day 9: Rope Bridge.
// dotnet fsi aoc09.fsx

open System.IO
open System.Text.RegularExpressions

type Direction = U | L | D | R

type Pos = (int * int)

let toDirection s =  
    match s with 
    | "U" -> U
    | "L" -> L
    | "D" -> D
    | "R" -> R
    | _ -> failwith <| sprintf "invalid direction %s" s

let toDirections motion = 
    match motion with 
    | (d, s) -> [1 .. s] |> List.map (fun _ -> d)

let tryParseMotion (s : string) = 
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
    | U -> (x, y+1)
    | L -> (x-1, y)
    | D -> (x, y-1)
    | R -> (x+1, y)

let toKnotMove (hx, hy) (tx, ty) : Pos = 
    match (hx - tx, hy - ty) with 
    | (-2,  2) -> (-1,  1)
    | (-1,  2) -> (-1,  1)
    | ( 0,  2) -> ( 0,  1)
    | ( 1,  2) -> ( 1,  1)
    | ( 2,  2) -> ( 1,  1)
    | ( 2,  1) -> ( 1,  1)
    | ( 2,  0) -> ( 1,  0)
    | ( 2, -1) -> ( 1, -1)
    | ( 2, -2) -> ( 1, -1)
    | ( 1, -2) -> ( 1, -1)
    | ( 0, -2) -> ( 0, -1)
    | (-1, -2) -> (-1, -1)
    | (-2, -2) -> (-1, -1)
    | (-2, -1) -> (-1, -1)
    | (-2,  0) -> (-1,  0)
    | (-2,  1) -> (-1,  1)
    | _ -> (0, 0)

let updateKnot (hx, hy) (tx, ty) : Pos = 
    match toKnotMove (hx, hy) (tx, ty)  with 
    | (dx, dy) -> (tx + dx, ty + dy)

let updateRope (dir : Direction) (rope : Pos list) : Pos list = 
    let rec fn (prev : Pos) (rope : Pos list) = 
        match rope with 
        | [] -> []
        | knot :: rest -> 
            let knot' = updateKnot prev knot
            knot' :: fn knot' rest
    match rope with 
    | [] -> [] 
    | head :: tail -> 
        let h' = (head |> updateHead dir)
        h' :: fn h' tail

let moveRope (ropeLength : int) (dirs : Direction list) = 
    let rec fn (step : int) (rope : Pos list) (visited : Set<Pos>) (dirs : Direction list) : Set<Pos> = 
        match dirs with 
        | [] -> 
            visited
        | d :: rest -> 
            let rope' = rope |> updateRope d 
            let lastPos = rope' |> List.rev |> List.head 
            let visited' = visited |> Set.add lastPos
            fn (step + 1) rope' visited' rest 
    let startRope = [1 .. ropeLength] |> List.map (fun _ -> (0, 0))
    let positions = fn 1 startRope Set.empty dirs
    positions |> Set.count

let run dirs = 
    dirs |> moveRope 2 |> printfn "Visited: %d"
    dirs |> moveRope 10 |> printfn "Visited: %d"

"input"
|> File.ReadAllLines
|> Array.toList 
|> List.choose tryParseMotion
|> List.collect toDirections
|> run 
