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
        let key = $"{device}-{dac}-{fft}"
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
    Map.empty |> loop start dac fft

let createColorMap devices lookupKeys = 
    let chooseColor d = 
        if lookupKeys |> List.contains $"{d}-True-True" then 
            "blue"
        else if lookupKeys |> List.contains $"{d}-True-False" then 
            "green"
        else if lookupKeys |> List.contains $"{d}-False-True" then 
            "purple"
        else 
            "yellow"

    devices |> List.map (fun d -> d, chooseColor d) |> Map.ofList

let run fileName = 
    let lines = readLines fileName
    let flow = lines |> Array.map parse |> Map.ofArray
    // solve "you" true true flow |> printfn "%d"
    let (lookup, count) = solve "svr" false false flow 
    let devices = Map.keys flow |> Seq.toList
    let lookupKeys = Map.keys lookup |> Seq.toList
    // printfn "%A" lookupKeys
    let colorMap = createColorMap devices lookupKeys 
    printfn "%A" colorMap
    0

run "input.txt"
