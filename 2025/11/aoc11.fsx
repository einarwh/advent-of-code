// Advent of Code 2025. Day 11: Reactor.
// dotnet fsi aoc11.fsx

open System
open System.IO

let parse (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> a, b.Split " "
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let solve start dac fft flow = 
    let rec loop device dac fft lookup = 
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
                let lookup', count = Array.fold folder (lookup, 0) devices 
                lookup' |> Map.add key count, count
    Map.empty |> loop start dac fft |> snd

let run fileName = 
    let lines = readLines fileName
    let flow = lines |> Array.map parse |> Map.ofArray
    solve "you" true true flow |> printfn "%d"
    solve "svr" false false flow |> printfn "%d"

run "input.txt"
