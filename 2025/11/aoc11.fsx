// Advent of Code 2025. Day 11: Reactor.
// dotnet fsi aoc11.fsx

open System
open System.IO

let parse (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> a, b.Split " " |> Array.toList
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let solve (start : string) (dac : bool) (fft : bool) (flow : Map<string, string list>) = 
    let rec loop (device : string) (dac : bool) (fft : bool) (lookup : Map<string, int64>)  = 
        let key = $"{device}{dac}{fft}"
        if device = "out" then 
            lookup, if dac && fft then 1L else 0L
        else
            match Map.tryFind key lookup with 
            | Some count -> lookup, count
            | None -> 
                let devices = Map.find device flow 
                let folder (m, acc) d = 
                    let m', c = m |> loop d (dac || d = "dac") (fft || d = "fft")
                    m', acc + c
                let lookup', count = List.fold folder (lookup, 0) devices 
                lookup' |> Map.add key count, count
    Map.empty |> loop start dac fft |> snd

let run fileName = 
    let lines = readLines fileName
    let map = lines |> List.map parse |> Map.ofList
    solve "you" true true map |> printfn "%d"
    solve "svr" false false map |> printfn "%d"

run "input.txt"
