// Advent of Code 2016. Day 01: No Time for a Taxicab.
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

let nextPosStep (x, y) dir =
    match dir with
    | N -> (x, y + 1)
    | W -> (x - 1, y)
    | S -> (x, y - 1)
    | E -> (x + 1, y)

let expand (dir : Direction, steps : int) : Direction list = 
    let rec fn acc stepsLeft =
        if stepsLeft > 0 then fn (dir :: acc) (stepsLeft - 1)
        else acc 
    fn [] steps

let rec toDirections (directions : Direction list) (dir : Direction) (relativeMoves : (Turn * int) list) = 
    match relativeMoves with 
    | [] -> directions |> List.rev 
    | (turn, steps) :: rest -> 
        let dir' = nextDir dir turn 
        toDirections (expand (dir', steps) @ directions) dir' rest 

let move (directions : Direction list) = 
    let rec fn (pos : int * int) (dirs : Direction list) = 
        match dirs with 
        | [] -> pos 
        | dir :: rest -> 
            let pos' = nextPosStep pos dir
            fn pos' rest 
    fn (0, 0) directions

let twice (directions : Direction list) = 
    let rec fn (seen : Set<int * int>) (pos : int * int) (dirs : Direction list) = 
        if seen |> Set.contains pos then 
            pos 
        else 
            match dirs with 
            | [] -> failwith "no hq" 
            | dir :: rest -> 
                let seen' = seen |> Set.add pos
                let pos' = nextPosStep pos dir
                fn seen' pos' rest 
    fn Set.empty (0, 0) directions

let readText fileName =
    File.ReadAllText(fileName).Trim()

let run fileName =
    let text = readText fileName
    let moves = text.Split(", ") |> List.ofArray |> List.map parse
    let directions = toDirections [] N moves
    let toDistance (x, y) = printfn "%d" (abs x + abs y)
    directions |> move |> toDistance 
    directions |> twice |> toDistance

run "input"
