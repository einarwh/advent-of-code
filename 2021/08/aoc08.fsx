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
    let five = sizeFiveDigits |> List.find (fun digit -> 3 = (Set.intersect digit four |> Set.count))
    let two = sizeFiveDigits |> List.find (fun digit -> digit <> three && digit <> five)
    
    printfn "6: %A" six
    printfn "9: %A" nine
    printfn "0: %A" zero
    let lookup = 
        [ (zero, 0); (one, 1); (two, 2); (three, 3); (four, 4); (five, 5); (six, 6); (seven, 7); (eight, 8); (nine, 9) ]
        |> Map.ofList 

    let digits = outputs |> List.map (fun digit -> Map.find digit lookup) 
    let red = digits |> List.reduce (fun a b -> 10 * a + b)
    printfn "%A" digits
    printfn "%d" red
    lookup

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
