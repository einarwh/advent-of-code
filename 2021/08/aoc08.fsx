// Advent of Code 2021. Day 08: Seven Segment Search.
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

let rec pow a b = 
    if b < 1 then 1 else a * pow a (b - 1)

let solveLine (signals : Set<char> list, outputs : Set<char> list) = 
    let sizeMap = signals |> List.groupBy (Set.count) |> Map.ofList 
    let getBySize n = sizeMap |> Map.find n |> List.head 
    let one = getBySize 2
    let four = getBySize 4
    let seven = getBySize 3
    let eight = getBySize 7
    let sizeSixDigits = sizeMap |> Map.find 6 
    let six = sizeSixDigits |> List.find (fun digit -> 1 = (Set.intersect digit one |> Set.count))
    let nine = sizeSixDigits |> List.find (fun digit -> 4 = (Set.intersect digit four |> Set.count))
    let zero = sizeSixDigits |> List.find (fun digit -> digit <> six && digit <> nine)
    let sizeFiveDigits = sizeMap |> Map.find 5 
    let three = sizeFiveDigits |> List.find (fun digit -> 2 = (Set.intersect digit one |> Set.count))
    let two = sizeFiveDigits |> List.find (fun digit -> 2 = (Set.intersect digit four |> Set.count))
    let five = sizeFiveDigits |> List.find (fun digit -> digit <> three && digit <> two)
    let lookup = 
        [ (zero, 0); (one, 1); (two, 2); (three, 3); (four, 4); (five, 5); (six, 6); (seven, 7); (eight, 8); (nine, 9) ]
        |> Map.ofList 
    outputs 
    |> List.map (fun digit -> Map.find digit lookup) 
    |> List.rev
    |> List.mapi (fun i x -> (pow 10 i) * x)
    |> List.sum 

let run fileName = 
    let lines = readLines fileName |> List.map parseLine 
    // Part 1 
    lines 
    |> List.collect snd
    |> List.map Set.count
    |> List.filter (fun len -> len = 2 || len = 3 || len = 4 || len = 7)
    |> List.length 
    |> printfn "%d"
    // Part 2 
    lines 
    |> List.map solveLine 
    |> List.sum 
    |> printfn "%d"

run "input.txt"
