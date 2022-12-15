// Advent of Code 2022. 
// Day 14: Regolith Reservoir.
// dotnet fsi aoc14.fsx

open System.IO 

type Pos = (int * int)

type Line = (Pos * Pos)

let toPos (s : string) = 
    match s.Split(",") with 
    | [|a;b|] -> (int a, int b)
    | _ -> failwith "not a valid position"

let toPositions (pos1 : Pos, pos2 : Pos) : Pos list = 
    let (startPos, endPos) = if pos1 < pos2 then (pos1, pos2) else (pos2, pos1)
    match (startPos, endPos) with 
    | ((x1, y1), (x2, y2)) -> 
        if x1 = x2 then 
            [y1 .. y2] |> List.map (fun y -> (x1, y))
        else 
            [x1 .. x2] |> List.map (fun x -> (x, y1))

let parseLine (s : string) = 
    s.Split(" -> ")
    |> Array.toList 
    |> List.map toPos
    |> List.pairwise
    |> List.collect toPositions
    |> Set.ofList

let findBottom (rocks : Set<Pos>) : int = 
    rocks |> Set.toList |> List.map snd |> List.max

let pourSandUntilVoid (rocks : Set<Pos>) = 
    let bottom = findBottom rocks 
    let rec tryFindPosition (pos : Pos) (occupied : Set<Pos>) : Pos option = 
        match pos with 
        | (x, y) ->
            if y = bottom then None
            else 
                let candidates = [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
                let maybe = candidates |> List.tryFind (fun c -> not <| Set.contains c occupied) 
                match maybe with 
                | Some nextPos -> 
                    tryFindPosition nextPos occupied
                | None -> 
                    Some pos 
    let rec pourSand (occupied : Set<Pos>) = 
        let startPos = (500, 0)
        match tryFindPosition startPos occupied with 
        | Some pos -> 
            if pos = startPos then 
                failwith "never reached the bottomless pit"
            else 
                let occupied' = occupied |> Set.add pos 
                if Set.count occupied = Set.count occupied' then 
                    failwith "shouldn't happen"
                else 
                    occupied' |> pourSand
        | None -> 
            occupied
    let occupied = pourSand rocks 
    let sand = Set.difference occupied rocks 
    Set.count sand

let run (lines : string list) = 
    let rocks = 
        lines
        |> List.map parseLine
        |> List.map (Set.toList)
        |> List.concat
        |> Set.ofList
    pourSandUntilVoid rocks |> printfn "Sand: %d"

"input"
|> File.ReadAllLines
|> Array.toList 
|> run 
