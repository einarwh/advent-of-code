// Advent of Code 2023. Day 18: Lavaduct Lagoon
// dotnet fsi aoc18.fsx

open System
open System.IO

type Direction = U | D | L | R 

type Pos = { x : int64; y : int64 }

type Vector = { start : Pos; stop : Pos }

type Instruction = {
    dir : Direction
    meters : int64
}

let directionFromString (s : string) =
    match s[0] with
    | 'U' -> U
    | 'D' -> D
    | 'L' -> L
    | 'R' -> R
    | _ -> failwith <| sprintf "%s?" s

let directionFromNumber (n : int) =
    match n with
    | 0 -> R 
    | 1 -> D
    | 2 -> L
    | 3 -> U
    | _ -> failwith <| sprintf "%d?" n

let parseHexCode (s : string) =
    let code = s.Substring(2, 5)
    let meters = Convert.ToInt32(code, 16)
    let dir = s.Substring(7, 1) |> int |> directionFromNumber
    { dir = dir
      meters = meters }

let parseInstruction (s : string) =
    let parts = s.Split(" ")
    { dir = directionFromString parts[0]
      meters = int parts[1] }

let parseLine (s : string) =
    let parts = s.Split(" ")
    parseHexCode parts[2]

let toStep dir = 
    match dir with 
    | U -> (0L, -1L)
    | D -> (0L, 1L)
    | L -> (-1L, 0L)
    | R -> (1L, 0L)

let getNextPos pos dir meters = 
    let (xStep, yStep) = toStep dir 
    { x = pos.x + xStep * meters
      y = pos.y + yStep * meters }

let toPositions instructions = 
    let rec loop pos (instructions : Instruction list) positions = 
        match instructions with 
        | [] -> positions |> List.rev
        | h :: t -> 
            let next = getNextPos pos h.dir h.meters
            loop next t (next :: positions)
    let startPos = { x = 0; y = 0 }
    loop startPos instructions [ startPos ] 

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let determinant (p1 : Pos, p2 : Pos) =
    p1.x * p2.y - p2.x * p1.y

let shoelaceArea positions = 
    positions |> List.pairwise |> List.map determinant |> List.sum |> (fun s -> s / 2L)

let solve instructions = 
    let positions = instructions |> toPositions
    let area = shoelaceArea positions
    let perimeter = instructions |> List.sumBy (fun x -> x.meters) 
    area + perimeter / 2L + 1L 

let part1 lines = 
    lines |> List.map parseInstruction |> solve 

let part2 lines =
    lines |> List.map parseLine |> solve

let run fileName =
    let lines = readLines fileName |> Array.toList
    lines |> part1 |> printfn "%d"
    lines |> part2 |> printfn "%d"

"input" |> run
