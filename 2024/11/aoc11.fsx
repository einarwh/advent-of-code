// Advent of Code 2024. Day 11: Plutonian Pebbles.
// dotnet fsi aoc11.fsx

open System.IO

let applyRules stone = 
    if stone = 0L then [1L]
    else 
        let s = stone.ToString()
        if s.Length % 2 = 0 then 
            let s1 = s.Substring(0, s.Length / 2)
            let s2 = s.Substring(s.Length / 2)
            [ s1; s2 ] |> List.map int64
        else
            [stone * 2024L]

let generator counted = 
    let compacted = 
        counted 
        |> List.collect (fun (stone, count) -> applyRules stone |> List.map (fun s -> (s, count)))
        |> List.groupBy (fun (s, n) -> s)
        |> List.map (fun (s, lst) -> (s, lst |> List.map snd |> List.sum))
    Some (compacted, compacted)

let blinking times stones = 
    stones 
    |> List.countBy id 
    |> List.map (fun (s, c) -> (s, int64 c))
    |> Seq.unfold generator  
    |> Seq.take times 
    |> Seq.last 
    |> List.map snd 
    |> List.sum

let run fileName = 
    let text = File.ReadAllText fileName 
    let stones = text.Trim().Split(" ") |> Array.toList |> List.map int64 
    stones |> blinking 25 |> printfn "%d"
    stones |> blinking 75 |> printfn "%d"

run "input.txt"
