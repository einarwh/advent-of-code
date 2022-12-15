// Advent of Code 2022. 
// Day 14: Regolith Reservoir.
// dotnet fsi aoc14.fsx

open System.IO 

type Pos = (int * int)

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

let pourSand (floor : bool) (bottom : int) (rocks : Set<Pos>) = 
    let rec tryFindPosition (pos : Pos) (occupied : Set<Pos>) : Pos option = 
        match pos with 
        | (x, y) ->
            if y = bottom && not floor then None
            else 
                let candidates = 
                    if y = bottom + 1 && floor then 
                        []
                    else 
                        [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
                let maybe = candidates |> List.tryFind (fun c -> not <| Set.contains c occupied) 
                match maybe with 
                | Some nextPos -> 
                    tryFindPosition nextPos occupied
                | None -> 
                    Some pos 
    let rec pour (occupied : Set<Pos>) = 
        let startPos = (500, 0)
        match tryFindPosition startPos occupied with 
        | Some pos -> 
            if pos = startPos then 
                occupied |> Set.add pos
            else 
                let occupied' = occupied |> Set.add pos 
                if Set.count occupied = Set.count occupied' then 
                    occupied
                else 
                    occupied' |> pour
        | None -> 
            occupied
    let occupied = pour rocks 
    let sand = Set.difference occupied rocks 
    let xs = sand |> Set.toList |> List.map fst
    printfn "x  %d -> %d" (xs |> List.min) (xs |> List.max)
    let ys = sand |> Set.toList |> List.map snd
    printfn "y  %d -> %d" (ys |> List.min) (ys |> List.max)
    Set.count sand

let part1 (rocks : Set<Pos>) = 
    let bottom = findBottom rocks 
    pourSand false bottom rocks |> printfn "Sand: %d"

let part2 (rocks : Set<Pos>) = 
    let bottom = findBottom rocks 
    pourSand true bottom rocks |> printfn "Sand: %d"

let run (lines : string list) = 
    let rocks = 
        lines
        |> List.map parseLine
        |> List.map (Set.toList)
        |> List.concat
        |> Set.ofList
    rocks |> part1 
    rocks |> part2 

"input"
|> File.ReadAllLines
|> Array.toList 
|> run 
