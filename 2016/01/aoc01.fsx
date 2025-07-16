// Advent of Code 2016. Day 01: No Time for a Taxicab
// dotnet fsi aoc01.fsx

open System
open System.IO

type Direction = N | W | S | E 

type Turn = R | L 

let parse (s : string) : (Turn * int) = 
    let ch = s[0]
    let n = Int32.Parse(s.Substring(1))
    if ch = 'R' then (R, n)
    else (L, n)

let nextDir dir turn = 
    match (dir, turn) with 
    | (N, R) -> E
    | (N, L) -> W
    | (W, R) -> N
    | (W, L) -> S
    | (S, R) -> W
    | (S, L) -> E
    | (E, R) -> S
    | (E, L) -> N

let nextPos (x, y) steps dir = 
    match dir with 
    | N -> (x, y + steps)
    | W -> (x - steps, y)
    | S -> (x, y - steps)
    | E -> (x + steps, y)

let rec move (pos : int * int) (dir : Direction) (moves : (Turn * int) list) = 
    match moves with 
    | [] -> pos 
    | (turn, steps) :: rest -> 
        let dir' = nextDir dir turn 
        let pos' = nextPos pos steps dir' 
        move pos' dir' rest

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    let moves = text.Split(", ") |> List.ofArray |> List.map parse
    let (x, y) = move (0, 0) N moves 
    printfn "%d" (x + y)

run "input"
