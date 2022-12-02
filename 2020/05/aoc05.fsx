// Advent of Code 2020. Day 5.
// dotnet fsi aoc05.fsx

open System.IO

type Step = Lower | Upper

let parse (lowerChar : char) (s : string) : Step list =
    s
    |> Seq.map (fun c -> if c = lowerChar then Lower else Upper)
    |> Seq.toList 

let search (num : int) (steps : Step list) : int =
    let rec fn (min : int) max steps =
        let mid = min + (max - min) / 2
        match steps with
        | [] -> min
        | Lower::t -> fn min mid t
        | Upper::t -> fn (mid + 1) max t
    fn 0 (num - 1) steps

let index (num : int) (lowerChar : char) (s : string) : int =
    s |> parse lowerChar |> search num 

let getSeatId (s : string) : int =
    let row = s.Substring(0, 7) |> index 128 'F'
    let col = s.Substring(7) |> index 8 'L'
    row * 8 + col

let findMySeat (seatIds : int seq) =
    seatIds
    |> Seq.pairwise
    |> Seq.pick (fun (lo, hi) -> if hi - lo = 2 then Some (lo + 1) else None)
 
let run lines =
    let seatIds = lines |> Seq.map getSeatId |> Seq.sort
    seatIds |> Seq.last |> printfn "Highest seat ID %d"
    seatIds |> findMySeat |> printfn "My seat ID %d"

"input" |> File.ReadLines |> run 