// Advent of Code 2023. Day 7: Camel Cards
// dotnet fsi aoc07.fsx

open System
open System.IO

let parseLine (line : string) = 
    match line.Split(" ") with 
    | [|card;bid|] -> (card, int bid)
    | _ -> failwith "Wrong"

let countCards = 
    List.countBy id >> List.map snd >> List.sortDescending

let rateCardCounts cardCounts = 
    match cardCounts with 
    | [5] -> 6
    | [4;1] -> 5
    | [3;2] -> 4
    | [3;1;1] -> 3
    | [2;2;1] -> 2
    | [2;1;1;1] -> 1
    | _ -> 0

let rateHand hand = 
    hand |> Seq.toList |> countCards |> rateCardCounts

let rateHandJoker hand = 
    let (jokers, rest) = hand |> Seq.toList |> List.partition ((=) 'J')
    let improved = 
        match countCards rest with 
        | h :: t -> (h + List.length jokers) :: t
        | _ -> [5]
    improved |> rateCardCounts

let rateCard card =
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

let rateCardJoker card =
    match card with 
    | 'J' -> 1
    | _ -> rateCard card

let compareByHandType handRater hand1 hand2 = 
    handRater hand1 - handRater hand2 

let compareByStrongestCard cardRater hand1 hand2 = 
    Seq.zip (Seq.map cardRater hand1) (Seq.map cardRater hand2) 
    |> Seq.map (fun (a, b) -> (a - b)) 
    |> Seq.tryFind (fun n -> n <> 0)
    |> Option.defaultValue 0

let compareHands handRater cardRater hand1 hand2 =
    match compareByHandType handRater hand1 hand2 with 
    | 0 -> compareByStrongestCard cardRater hand1 hand2 
    | n -> n

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let runWith handRater cardRater parsedLines = 
    let comparer = compareHands handRater cardRater
    parsedLines 
    |> List.sortWith (fun (h1,_) (h2,_) -> comparer h1 h2)
    |> List.map snd
    |> List.mapi (fun i bid -> (i + 1) * bid)
    |> List.sum

let run fileName = 
    let parsed = readLines fileName |> Array.toList |> List.map parseLine
    parsed |> runWith rateHand rateCard |> printfn "%d"
    parsed |> runWith rateHandJoker rateCardJoker |> printfn "%d"

"input" |> run 
