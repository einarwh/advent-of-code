// Advent of Code 2025. Day 03: Lobby.
// dotnet fsi aoc03.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let findMaxJoltage batteries bank = 
    let rec find joltage batteries bank = 
        if batteries = 0 then joltage
        else 
            let ix, battery = 
                bank 
                |> List.take (List.length bank - batteries + 1)
                |> List.mapi (fun i d -> i, d) 
                |> List.maxBy snd 
            find (joltage * 10L + battery) (batteries - 1) (List.skip (ix + 1) bank)
    find 0 batteries bank 

let run fileName = 
    let toDigits = Seq.toList >> List.map (Char.GetNumericValue >> int64)
    let digits = fileName |> readLines |> List.map toDigits 
    digits |> List.sumBy (findMaxJoltage 2) |> printfn "%d" 
    digits |> List.sumBy (findMaxJoltage 12) |> printfn "%d" 

run "input.txt"
