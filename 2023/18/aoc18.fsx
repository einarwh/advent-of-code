// Advent of Code 2023. Day 18: Lavaduct Lagoon
// dotnet fsi aoc18.fsx

open System
open System.IO

type Direction = (int * int)

type Instruction = {
    dir : Direction
    meters : int 
    color : string 
}

let parseDirection (s : string) = 
    match s[0] with 
    | 'U' -> (0, -1)
    | 'D' -> (0, 1)
    | 'L' -> (-1, 0) 
    | 'R' -> (1, 0) 
    | _ -> failwith <| sprintf "%s?" s

let parseMeters = int 

let parseColor = id 

let parseInstruction (s : string) = 
    let parts = s.Split(" ")
    { dir = parseDirection parts[0]
      meters = parseMeters parts[1]
      color = parseColor parts[2] }

let move (xStart, yStart) (xStep, yStep) meters =
    [ 1 .. meters ]
    |> List.map (fun m -> (xStart + xStep * m, yStart + yStep * m))

let digLagoon (instructions : Instruction list) = 
    let startPos = (0, 0)
    let rec loop instructions current positions = 
        match instructions with 
        | [] -> positions
        | h :: t -> 
            if List.contains startPos positions then failwith "oh hey"
            let nextPositions = move current h.dir h.meters 
            printfn "curr: %A" current
            printfn "step %d in dir %A" h.meters h.dir
            printfn "next: %A" nextPositions
            let next = nextPositions |> List.last 
            loop t next (positions @ nextPositions)
    loop instructions startPos []

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName |> Array.toList 
    let instructions = lines |> List.map parseInstruction
    let lagoon = instructions |> digLagoon
    lagoon |> List.length |> printfn "%d"
    lagoon |> printfn "%A"
    ()

"sample" |> run
