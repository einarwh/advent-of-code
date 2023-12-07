// Advent of Code 2023. 
// dotnet fsi aoc07.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parseLine (line : string) : (string * int64) = 
    match line.Split(" ") with 
    | [|card;bid|] ->
        (card, int64 bid)
    | _ -> failwith "Wrong"

let rateHandType (hand : string) = 
    let lst = 
        hand 
        |> Seq.toList
        |> List.groupBy id
        |> List.map (snd >> List.length)
        |> List.sortDescending
    match lst with 
    | [5] -> 6
    | [4;1] -> 5
    | [3;2] -> 4
    | [3;1;1] -> 3
    | [2;2;1] -> 2
    | [2;1;1;1] -> 1
    | _ -> 0

let rateCard (card : char) : int =
    match card with 
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "Wrong"

let compareByHandType (hand1 : string) (hand2 : string) = 
    rateHandType hand1 - rateHandType hand2 

let compareByStrongestCard (hand1 : string) (hand2 : string) = 
    Seq.zip (Seq.map rateCard hand1) (Seq.map rateCard hand2) 
    |> Seq.map (fun (a, b) -> (a - b)) 
    |> Seq.tryFind (fun n -> n <> 0)
    |> Option.defaultValue 0

let compareHands (hand1 : string) (hand2 : string) =
    match compareByHandType hand1 hand2 with 
    | 0 -> compareByStrongestCard hand1 hand2 
    | n -> n

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let parsed = lines |> List.map parseLine 
    let sorted = 
        parsed 
        |> List.sortWith (fun (h1,b1) (h2, b2) -> compareHands h1 h2)
    let winnings =
        sorted
        |> List.map snd
        |> List.mapi (fun i bid -> (int64 i + 1L) * bid)
        |> List.sum
    printfn "%d" winnings

"input" |> run 
