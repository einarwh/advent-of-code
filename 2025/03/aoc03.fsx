// Advent of Code 2025. Day 03.
// dotnet fsi aoc03.fsx

open System
open System.IO

let toDigits (s : string) = 
    s |> Seq.toList |> List.map (Char.GetNumericValue >> int64)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let toJoltage bank : int64 =
    let rec fn acc bank = 
        match bank with 
        | [] -> acc 
        | h :: t -> 
            fn (acc * 10L + h) t 
    fn 0L bank 

let rec findBest (batteries : int) (joltage : int64) bank = 
    // printfn "findBest %d %d %A" batteries joltage bank
    if batteries = 0 then Some joltage 
    else if List.length bank < batteries then None 
    else 
        match bank with 
        | [] -> None 
        | h :: t -> 
            let r1 = findBest batteries joltage t 
            let r2 = findBest (batteries - 1) (joltage * 10L + h) t
            max r1 r2 

let findMax (batteries : int) bank = 
    printfn "findMax %d %A" batteries bank
    findBest batteries 0 bank

let run fileName = 
    let lines = readLines fileName
    let digits = lines |> List.map toDigits 
    // digits |> List.map (findJoltages 12) |> printfn "%A" 
    digits |> List.choose (findMax 12) |> List.sum |> printfn "%d" 

run "sample.txt"
