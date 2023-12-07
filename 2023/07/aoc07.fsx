// Advent of Code 2023. 
// dotnet fsi aoc07.fsx

open System
open System.IO

let parseLine (line : string) : (string * int64) = 
    match line.Split(" ") with 
    | [|card;bid|] ->
        (card, int64 bid)
    | _ -> failwith "Wrong"

let rateHand1 (hand : string) = 
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

let rateHand2 (hand : string) = 
    let (jokers, rest) = hand |> Seq.toList |> List.partition (fun ch -> ch = 'J')
    let jokerCount = jokers |> List.length
    let lst = 
        rest 
        |> Seq.toList
        |> List.groupBy id
        |> List.map (snd >> List.length)
        |> List.sortDescending
    let improved = 
        match lst with 
        | h :: t -> (h + jokerCount) :: t
        | _ -> lst
    match improved with 
    | [5] -> 6
    | [4;1] -> 5
    | [3;2] -> 4
    | [3;1;1] -> 3
    | [2;2;1] -> 2
    | [2;1;1;1] -> 1
    | _ -> 0

let rateCard1 (card : char) : int =
    match card with 
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

let rateCard2 (card : char) : int =
    match card with 
    | 'J' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "Wrong"

let compareByHandType handTypeRater (hand1 : string) (hand2 : string) = 
    handTypeRater hand1 - handTypeRater hand2 

let compareByStrongestCard cardRater (hand1 : string) (hand2 : string) = 
    Seq.zip (Seq.map cardRater hand1) (Seq.map cardRater hand2) 
    |> Seq.map (fun (a, b) -> (a - b)) 
    |> Seq.tryFind (fun n -> n <> 0)
    |> Option.defaultValue 0

let compareHands handRater cardRater (hand1 : string) (hand2 : string) =
    match compareByHandType handRater hand1 hand2 with 
    | 0 -> compareByStrongestCard cardRater hand1 hand2 
    | n -> n

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let runWith comparer parsedLines = 
    parsedLines 
    |> List.sortWith (fun (h1,_) (h2,_) -> comparer h1 h2)
    |> List.map snd
    |> List.mapi (fun i bid -> (int64 i + 1L) * bid)
    |> List.sum

let run fileName = 
    let parsed = readLines fileName |> Array.toList |> List.map parseLine
    parsed |> runWith (compareHands rateHand1 rateCard1) |> printfn "%d"
    parsed |> runWith (compareHands rateHand2 rateCard2) |> printfn "%d"

"input" |> run 
