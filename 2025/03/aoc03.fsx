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

let findMaxJoltage bank =
    let findMax bank = 
        match bank with 
        | [] -> None 
        | [ _ ] -> None 
        | h :: t -> 
            Some (h * 10L + List.max t)
    let rec fn acc bank = 
        match bank with 
        | [] -> acc 
        | [ _ ] -> acc 
        | _ :: t -> 
            let acc' = findMax bank :: acc 
            fn acc' t 
    let all = fn [] bank |> List.choose id 
    all |> List.max 

let removeLowest toRemove (bank : int64 list) = 
    let lowest = bank |> List.min 
    let rec fn acc toRemove bank = 
        // printfn "... %d" toRemove
        if toRemove > 0 then 
            match bank with 
            | [] -> acc |> List.rev 
            | h :: t when h = lowest -> 
                // printfn "removing..."
                fn acc (toRemove - 1) t 
            | h :: t -> 
                fn (h :: acc) toRemove t 
        else (acc |> List.rev) @ bank
    fn [] toRemove bank 

let rec findMaxByRemoving (batteries : int) (bank : int64 list) = 
    let toRemove = List.length bank - batteries 
    if toRemove > 0 then 
        removeLowest toRemove bank |> findMaxByRemoving batteries 
    else 
        bank 

let toJoltage bank : int64 =
    let rec fn acc bank = 
        match bank with 
        | [] -> acc 
        | h :: t -> 
            fn (acc * 10L + h) t 
    fn 0L bank 

let findMaxJoltage2 (batteries : int) bank =
    let rec fn acc (bank : int64 list) = 
        if List.length bank >= batteries then 
            fn (bank :: acc) (List.tail bank) 
        else 
            acc
    let all = fn [] bank 
    all |> List.map (findMaxByRemoving batteries) |> List.map toJoltage |> List.max 

let run fileName = 
    let lines = readLines fileName
    lines |> List.map toDigits |> List.sumBy (findMaxJoltage2 2) |> printfn "%d"
    // lines |> printfn "%A"
    // "987654321111111" |> toDigits |> printfn "%A"
    // "987654321111111" |> toDigits |> findMaxJoltage |> printfn "%d"
    // "811111111111119" |> toDigits |> findMaxJoltage |> printfn "%d"
    // "234234234234278" |> toDigits |> findMaxJoltage |> printfn "%d"
    // "818181911112111" |> toDigits |> findMaxJoltage |> printfn "%d"
    lines |> List.map toDigits |> List.sumBy (findMaxJoltage2 12) |> printfn "%d"
    // [8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1] |> findMaxByRemoving 12

run "sample.txt"
