// Advent of Code 2020. Day 10.
// dotnet fsi aoc10x.fsx

open System.IO

type Adapter = int
type Count = int64

let read (path : string) : Adapter array =
    File.ReadAllLines path
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map int 

let part1 (input : Adapter array) : int =
    let device = 3 + Array.max input
    let counted = 
        input
        |> Array.append [|0; device|]
        |> Array.sort
        |> Array.pairwise
        |> Array.map (fun (a, b) -> b - a)
        |> Array.countBy id
    let choose targetDiff (diff, count) =
        if diff = targetDiff then Some count else None
    let ones = counted |> Array.pick (choose 1) 
    let threes = counted |> Array.pick (choose 3)
    ones * threes

let rec arr (current : Adapter) (cache : Count option array) : Count =
    if current >= Array.length cache then
        0L
    else
        match cache.[current] with 
        | Some c -> c
        | None ->
            let c = [1..3] |> List.sumBy (fun i -> arr (current + i) cache)
            cache.[current] <- Some c
            c
            
let part2 (input : Adapter array) : Count =
    let device = 3 + Array.max input
    let input' = input |> Array.append [|0;device|] |> Array.sort
    let cache = [| for i in 0 .. device -> if Array.contains i input' then None else Some 0L |]
    cache.[device] <- Some 1L
    arr 0 cache 
    
let run input =
    input |> part1 |> printfn "Part 1: %d"
    input |> part2 |> printfn "Part 2: %d"

"input" |> read |> run