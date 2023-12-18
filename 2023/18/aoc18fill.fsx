// Advent of Code 2023. Day 18: Lavaduct Lagoon, Part 1.
// dotnet fsi aoc18fill.fsx

open System
open System.IO

type Instruction = {
    dir : (int * int)
    meters : int 
}

let parseDirection (s : string) = 
    match s[0] with 
    | 'U' -> (0, -1)
    | 'D' -> (0, 1)
    | 'L' -> (-1, 0) 
    | 'R' -> (1, 0) 
    | _ -> failwith <| sprintf "%s?" s

let parseInstruction (s : string) = 
    let parts = s.Split(" ")
    { dir = parseDirection parts[0]
      meters = int parts[1] }

let move (xStart, yStart) (xStep, yStep) meters =
    [ 1 .. meters ]
    |> List.map (fun m -> (xStart + xStep * m, yStart + yStep * m))

let digLagoon instructions = 
    let startPos = (0, 0)
    let rec loop instructions current positions = 
        match instructions with 
        | [] -> positions
        | h :: t -> 
            if List.contains startPos positions then failwith "oh hey"
            let nextPositions = move current h.dir h.meters 
            let next = nextPositions |> List.last 
            loop t next (positions @ nextPositions)
    loop instructions startPos []

let getNeighbours (x, y) = 
    [ (x - 1, y - 1)
      (x, y - 1) 
      (x + 1, y - 1)
      (x - 1, y)
      (x + 1, y)
      (x - 1, y + 1)
      (x, y + 1)
      (x + 1, y + 1) ]

let fill positions = 
    let wallSet = positions |> Set.ofList
    let rec loop (queue : (int*int) list) (visited : Set<int*int>) = 
        match queue with 
        | [] -> visited 
        | (x, y) :: rest -> 
            if Set.contains (x, y) visited then
                loop rest visited 
            else 
                let visited = visited |> Set.add (x, y)
                let neighbours = (x, y) |> getNeighbours |> List.filter (fun p -> not (Set.contains p wallSet))
                let queue = queue @ neighbours
                loop queue visited
    let xStart = positions |> List.map fst |> List.min
    let yStart = positions |> List.choose (fun (x, y) -> if x = xStart then Some y else None) |> List.min
    let startPos = (xStart+1, yStart+1)
    loop [ startPos ] Set.empty

let getVolume (positions : (int * int) list) = 
    let wallCount = positions |> List.length 
    let fillCount = positions |> fill |> Set.count 
    wallCount + fillCount

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName |> Array.toList 
    let instructions = lines |> List.map parseInstruction
    let lagoon = instructions |> digLagoon
    lagoon |> getVolume |> printfn "%d"

"input" |> run
