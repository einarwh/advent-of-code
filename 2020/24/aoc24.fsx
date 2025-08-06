// Advent of Code 2020. Day 24: Lobby Layout
// https://en.wikipedia.org/wiki/Hexagonal_Efficient_Coordinate_System
// dotnet fsi aoc24.fsx

open System
open System.IO

type Move = 
    | E
    | SE 
    | SW 
    | W 
    | NW 
    | NE 

type Hecs = {
    a : int 
    r : int 
    c : int 
}

let parseLine (s : string) : Move list =  
    let rec loop (moves : Move list) (chars : char list) = 
        match chars with 
        | [] -> List.rev moves 
        | ch :: rest when ch = 'w' -> loop (W :: moves) rest 
        | ch :: rest when ch = 'e' -> loop (E :: moves) rest 
        | ch1 :: ch2 :: rest when ch1 = 'n' && ch2 = 'w' -> loop (NW :: moves) rest 
        | ch1 :: ch2 :: rest when ch1 = 'n' && ch2 = 'e' -> loop (NE :: moves) rest 
        | ch1 :: ch2 :: rest when ch1 = 's' && ch2 = 'w' -> loop (SW :: moves) rest 
        | ch1 :: ch2 :: rest when ch1 = 's' && ch2 = 'e' -> loop (SE :: moves) rest 
        | _ -> failwith "?"
    s |> Seq.toList |> loop [] 

let adjacent (tile : Hecs) (move : Move) = 
    match move with 
    | E -> { tile with c = tile.c + 1 }
    | SE -> 
        if tile.a = 0 then 
            { tile with a = 1 } 
        else 
            { tile with a = 0; r = tile.r + 1; c = tile.c + 1 }
    | SW -> 
        if tile.a = 0 then 
            { tile with a = 1; c = tile.c - 1 } 
        else 
            { tile with a = 0; r = tile.r + 1 }
    | W -> { tile with c = tile.c - 1 }
    | NW -> 
        if tile.a = 0 then 
            { tile with a = 1; r = tile.r - 1; c = tile.c - 1 } 
        else 
            { tile with a = 0 }
    | NE -> 
        if tile.a = 0 then 
            { tile with a = 1; r = tile.r - 1 } 
        else 
            { tile with a = 0; c = tile.c + 1  }

let findTile (moves : Move list) = 
    let rec find (tile : Hecs) (moves : Move list) = 
        match moves with 
        | [] -> tile 
        | m :: rest -> 
            find (adjacent tile m) rest 
    find { a = 0; r = 0; c = 0 } moves

let getAdjacentTiles (tile : Hecs) : Hecs list = 
    [ adjacent tile E
    ; adjacent tile SE
    ; adjacent tile SW
    ; adjacent tile W 
    ; adjacent tile NW 
    ; adjacent tile NE ]

let findInitialBlackTiles (moveList : Move list list) = 
    moveList |> List.map findTile |> List.countBy id |> List.choose (fun (t, c) -> if c % 2 = 1 then Some t else None)

let findWhiteTiles (blackTiles : Set<Hecs>) : Set<Hecs> = 
    let neighbours = blackTiles |> Set.toList |> List.collect getAdjacentTiles |> Set.ofList 
    Set.difference neighbours blackTiles 

let evolveStep (blackTiles : Set<Hecs>) = 
    let whiteTiles = findWhiteTiles blackTiles 
    let remainsBlack (t : Hecs) : bool = 
        let count = getAdjacentTiles t |> List.filter (fun a -> Set.contains a blackTiles) |> List.length 
        count = 1 || count = 2 
    let becomesBlack (t : Hecs) : bool = 
        let count = getAdjacentTiles t |> List.filter (fun a -> Set.contains a blackTiles) |> List.length 
        count = 2 
    let black1 = blackTiles |> Set.filter remainsBlack
    let black2 = whiteTiles |> Set.filter becomesBlack
    Set.union black1 black2 

let evolve days blackTiles = 
    let rec loop day tiles = 
        if day < days then 
            loop (day + 1) (evolveStep tiles)
        else 
            tiles 
    loop 0 blackTiles 

let findMinMaxRow tiles = 
    let rec loop (lowest, highest) tiles = 
        match tiles with 
        | [] -> (lowest, highest) 
        | tile :: rest -> 
            loop (min lowest tile.r, max highest tile.r) rest 
    loop (0, 0) tiles 

let findMinMaxColumn tiles = 
    let rec loop (lowest, highest) tiles = 
        match tiles with 
        | [] -> (lowest, highest) 
        | tile :: rest -> 
            loop (min lowest tile.c, max highest tile.c) rest 
    loop (0, 0) tiles 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let moveList : Move list list = lines |> List.map parseLine 
    let blackTiles = findInitialBlackTiles moveList |> Set.ofList 
    blackTiles |> Set.count |> printfn "%d"
    let hundred = blackTiles |> evolve 100
    hundred |> Set.count |> printfn "%d"
    // findMinMaxRow (hundred |> Set.toList) |> printfn "%A" -- (-32, 31)
    // findMinMaxColumn (hundred |> Set.toList) |> printfn "%A" -- (-56, 59)

run "input.txt"
