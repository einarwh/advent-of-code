// Advent of Code 2024. Day 11: Plutonian Pebbles.
// dotnet fsi aoc11.fsx

open System
open System.IO

let applyRules stone = 
    if stone = 0L then 
        [1L]
    else 
        let s = stone.ToString()
        if s.Length % 2 = 0 then 
            let s1 = s.Substring(0, s.Length / 2)
            let s2 = s.Substring(s.Length / 2)
            [ s1; s2 ] |> List.map int64
        else
            [stone * 2024L]

let rec blink (memo : Map<int64 * int, int64 list>) (stone : int64, times : int) : (Map<int64 *int, int64 list> * int64 list) = 
    match memo |> Map.tryFind (stone, times) with 
    | Some result -> (memo, result)
    | None -> 
        let stones = applyRules stone 
        let folder (memo : Map<int64 * int, int64 list>, result : int64 list) (stone : int64) : (Map<int64 *int, int64 list> * int64 list) = 
            let (m, r) = blink memo (stone, times - 1)
            (m, result @ r)  
        let (m, r) = stones |> List.fold folder (memo, [])
        (m, r)

let blinking (times : int) (stones : int64 list) = 
    let memo : Map<int64 * int, int64 list> = Map.empty
    let rec loop (times : int) (stones : int64 list)

    let folder (memo : Map<int64 * int, int64 list>, result : int64 list) (stone : int64) : (Map<int64 *int, int64 list> * int64 list) = 
        let (m, r) = blink memo (stone, times - 1)
        (m, result @ r)  
    let (m, r) = stones |> List.fold folder (memo, [])
    r 

let run fileName = 
    let text = File.ReadAllText fileName 
    let stones = text.Trim().Split(" ") |> Array.toList |> List.map int64 
    stones |> blinking 25 |> List.length |> printfn "%d"
    // stones |> blinking 75 |> List.length |> printfn "%d"

run "input"
