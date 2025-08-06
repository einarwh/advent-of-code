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

let nextTile (tile : Hecs) (move : Move) = 
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
            find (nextTile tile m) rest 
    find { a = 0; r = 0; c = 0 } moves

let findInitialBlackTiles (moveList : Move list list) = 
    moveList |> List.map findTile |> List.countBy id |> List.filter (fun (t, c) -> c % 2 = 1)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let moveList : Move list list = lines |> List.map parseLine 
    let blackTiles = findInitialBlackTiles moveList 
    blackTiles |> List.length |> printfn "%d"

run "input.txt"
