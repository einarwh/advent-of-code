// Advent of Code 2025. Day 11: Reactor.
// dotnet fsi aoc11.fsx

open System
open System.IO

let parse (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> (a, b.Split " " |> Array.toList)
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let rec solvePart1 (device : string) (flow : Map<string, string list>) : int = 
    if device = "out" then 1 
    else 
        let devices = Map.find device flow 
        devices |> List.sumBy (fun d -> solvePart1 d flow)

let rec solvePaths (device : string) (dac : bool) (fft : bool) (lookup : Map<string, int64>) (flow : Map<string, string list>) = 
    let key = $"{device}{dac}{fft}"
    if device = "out" then 
        lookup, if dac && fft then 1L else 0L
    else
        match Map.tryFind key lookup with 
        | Some count -> lookup, count
        | None -> 
            let devices = Map.find device flow 
            let folder (m, acc) d = 
                let m', c = solvePaths d (dac || d = "dac") (fft || d = "fft") m flow 
                m', acc + c
            let lookup', count = List.fold folder (lookup, 0) devices 
            lookup' |> Map.add key count, count

// let solve (start : string) (dac : bool) (fft : bool) (flow : Map<string, string list>) = 

//     solvePaths 


let run fileName = 
    let lines = readLines fileName
    let map = lines |> List.map parse |> Map.ofList
    solve "you" map |> printfn "%d"
    solvePaths "svr" false false Map.empty map |> printfn "%A"

run "input.txt"
