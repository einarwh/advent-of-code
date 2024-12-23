// Advent of Code 2021. Day 08
// dotnet fsi aoc08.fsx

open System
open System.IO

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parse (s : string) = 
    s |> split " " |> Array.toList |> List.map (Set.ofSeq)

let parseLine (s : string) = 
    match s |> split " | " with 
    | [|signals;outputs|] -> (parse signals, parse outputs)
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let solveLine (signals : Set<char> list, outputs : Set<char> list) = 
    let sizeMap = signals |> List.groupBy (Set.count) |> Map.ofList 
    printfn "%A" sizeMap
    let digitMap = 
        Map.empty 
        |> Map.add 1 (Map.find 2 sizeMap |> List.head)
        |> Map.add 4 (Map.find 4 sizeMap |> List.head)
        |> Map.add 7 (Map.find 3 sizeMap |> List.head)
        |> Map.add 8 (Map.find 7 sizeMap |> List.head)
    let one = digitMap |> Map.find 1 
    let four = digitMap |> Map.find 4
    
    let sizeSixDigits = sizeMap |> Map.find 6 
    let six = sizeSixDigits |> List.find (fun digit -> 1 = (Set.intersect digit one |> Set.count))
    let nine = sizeSixDigits |> List.find (fun digit -> 4 = (Set.intersect digit four |> Set.count))
    let zero = sizeSixDigits |> List.find (fun digit -> digit <> six && digit <> nine)

    let sizeFiveDigits = sizeMap |> Map.find 5 
    let three = sizeFiveDigits |> List.find (fun digit -> 2 = (Set.intersect digit one |> Set.count))
    let five = sizeFiveDigits |> List.find (fun digit -> 3 = (Set.intersect digit four |> Set.count))
    let two = sizeFiveDigits |> List.find (fun digit -> digit <> three && digit <> five)
    
    printfn "6: %A" six
    printfn "9: %A" nine
    printfn "0: %A" zero
    0

let run fileName = 
    let lines = readLines fileName |> List.map parseLine 
    lines 
    |> List.collect snd
    |> List.map Set.count
    |> List.filter (fun len -> len = 2 || len = 3 || len = 4 || len = 7)
    |> List.length 
    |> printfn "%d"

    let foo = parseLine "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    solveLine foo

    0

run "input"
