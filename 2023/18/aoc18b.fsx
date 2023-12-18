// Advent of Code 2023. Day 18: Lavaduct Lagoon
// dotnet fsi aoc18.fsx

open System
open System.IO

type Direction = (int64 * int64)

type Pos = (int64 * int64)

type Instruction = {
    dir : Direction
    meters : int64 
}

let parseDirection (s : string) = 
    match s[0] with 
    | 'U' -> (0L, -1L)
    | 'D' -> (0L, 1L)
    | 'L' -> (-1L, 0L) 
    | 'R' -> (1L, 0L) 
    | _ -> failwith <| sprintf "%s?" s

let directionFromNumber (n : int) = 
    match n with 
    | 0 -> (1L, 0L)
    | 1 -> (0L, 1L)
    | 2 -> (-1L, 0L) 
    | 3 -> (0L, -1L) 
    | _ -> failwith <| sprintf "%d?" n 

let parseMeters = int64 

let parseHexCode (s : string) = 
    let code = s.Substring(2, 5)
    let meters = Convert.ToInt32(code, 16)
    let dir = s.Substring(7, 1) |> int |> directionFromNumber
    { dir = dir
      meters = meters }

let parseInstruction (s : string) = 
    let parts = s.Split(" ")
    { dir = parseDirection parts[0]
      meters = parseMeters parts[1] }

let parseLine (s : string) = 
    let parts = s.Split(" ")
    parseHexCode parts[2]

let move (xStart, yStart) (xStep, yStep) meters =
    [ 1L .. meters ]
    |> List.map (fun m -> (xStart + xStep * m, yStart + yStep * m))

let digLagoon instructions = 
    let startPos = (0L, 0L)
    let rec loop instructions current positions = 
        match instructions with 
        | [] -> positions
        | h :: t -> 
            if List.contains startPos positions then failwith "oh hey"
            let nextPositions = move current h.dir h.meters 
            let next = nextPositions |> List.last 
            loop t next (positions @ nextPositions)
    loop instructions startPos []

let getNeighbours (x : int64, y : int64) = 
    [ (x - 1L, y - 1L)
      (x, y - 1L) 
      (x + 1L, y - 1L)
      (x - 1L, y)
      (x + 1L, y)
      (x - 1L, y + 1L)
      (x, y + 1L)
      (x + 1L, y + 1L) ]

let fill positions = 
    let wallSet = positions |> Set.ofList
    let rec loop (queue : Pos list) (visited : Set<Pos>) = 
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
    let startPos = (xStart+1L, yStart+1L)
    loop [ startPos ] Set.empty

let getVolume (positions : Pos list) = 
    let wallCount = positions |> List.length 
    let fillCount = positions |> fill |> Set.count 
    wallCount + fillCount

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let part1 lines = 
    let instructions = lines |> List.map parseInstruction
    let lagoon = instructions |> digLagoon
    lagoon |> getVolume 

let part2 lines = 
    let instructions = lines |> List.map parseLine
    let lagoon = instructions |> digLagoon
    lagoon |> getVolume 

let rec gcd (x : int64) (y : int64) = 
    if y = 0 then abs x else gcd y (x % y)


let factors n = 
    let rec f n x a = 
        if x = n then
            x::a
        elif n % x = 0 then 
            f (n/x) x (x::a)
        else
            f n (x+1) a
    f n 2 []

let lst = 
  [ 461937
    56407
    356671
    863240
    367720
    266681
    577262
    829975
    112010
    829975
    491645
    686074
    5411
    500254 ]

let run fileName =
    let lines = readLines fileName |> Array.toList 
    lines |> part1 |> printfn "%d"
    // lines |> part2 |> printfn "%d"
    lst |> List.map factors |> List.zip lst |> List.iter (printfn "%A")
    // factors 461937 |> printfn "%A" 
    gcd 461937 5411 |> printfn "%d"

"sample" |> run
