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

let sequence countedStones = 
    let generator counted = 
        let next = counted |> List.collect (fun (stone, count) -> applyRules stone |> List.map (fun s -> (s, count)))
        let grouped = next |> List.groupBy (fun (s, n) -> s)
        let compacted = grouped |> List.map (fun (s, lst) -> (s, lst |> List.map snd |> List.sum))
        Some (compacted, compacted)
    countedStones |> Seq.unfold generator 

let blinking times stones = 
    stones 
    |> List.countBy id 
    |> List.map (fun (s, c) -> (s, int64 c))
    |> sequence 
    |> Seq.take times 
    |> Seq.last 
    |> List.map snd 
    |> List.sum

let run fileName = 
    let text = File.ReadAllText fileName 
    let stones = text.Trim().Split(" ") |> Array.toList |> List.map int64 
    stones |> blinking 25 |> printfn "%d"
    stones |> blinking 75 |> printfn "%d"
    // let counted = stones |> List.countBy id |> List.map (fun (s, c) -> (s, int64 c))
    // let blinked = counted |> sequence |> Seq.take 75 |> Seq.last 
    // printfn "%A" blinked
    // blinked |> List.map snd |> List.sum |> printfn "%d"
    // stones |> blinking 75 |> List.length |> printfn "%d"
    0

run "input"
