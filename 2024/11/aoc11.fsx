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

let blink stones = 
    stones |> List.collect applyRules

let rec blinking times stones = 
    if times > 0 then 
        stones |> blink |> blinking (times - 1)
    else 
        stones 

let run fileName = 
    let text = File.ReadAllText fileName 
    let stones = text.Trim().Split(" ") |> Array.toList |> List.map int64 
    stones |> blinking 25 |> List.length |> printfn "%d"
    // stones |> blinking 75 |> List.length |> printfn "%d"

run "input"
