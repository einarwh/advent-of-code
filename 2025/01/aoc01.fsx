// Advent of Code 2025. Day 01: Secret Entrance.
// dotnet fsi aoc01.fsx

open System.IO

let parse (s : string) = 
    let n = int <| s.Substring 1
    match s[0] with 
    | 'L' -> -n 
    | _ -> n

let run fileName = 
    let numbers = File.ReadAllLines fileName |> Array.map parse
    let zeroes (current, count) rot = 
        let next = (current + rot) % 100
        (next + 100) % 100, count + if next = 0 then 1 else 0
    let steps num = 
        if num < 0 then Seq.replicate (abs num) -1 else Seq.replicate num 1 
        |> Seq.toArray
    numbers |> Array.fold zeroes (50, 0) |> snd |> printfn "%d"
    numbers |> Array.collect steps |> Array.fold zeroes (50, 0) |> snd |> printfn "%d"

run "input.txt"
