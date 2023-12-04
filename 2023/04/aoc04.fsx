// Advent of Code 2023. Day 4: Scratchcards
// dotnet fsi aoc04.fsx

open System
open System.IO

type Card = { Number : int; Wins : int }

let split (sep : string) (s : string) = s.Split(sep)

let trim (s : string) = s.Trim()

let substring from (s : string) = s.Substring(from)

let isNonEmpty (s : string) = s.Length > 0  

let parseCard (line : string) : Card = 
    let s1 = split ": " line 
    let cardNo = s1[0] |> substring ("Card ".Length) |> int
    let s2 = split " | " s1[1]
    let winning = s2[0] |> split " " |> Array.filter isNonEmpty |> Array.map int 
    let numbers = s2[1] |> split " " |> Array.filter isNonEmpty |> Array.map int  
    let found = 
        Set.intersect (Set.ofArray winning) (Set.ofArray numbers) 
        |> Set.count
    { Number = cardNo; Wins = found }

let calculate (card : Card) : int = 
    match card.Wins with 
    | 0 -> 0 
    | n -> pown 2 (n - 1)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let cards = lines |> List.map parseCard
    cards 
    |> List.sumBy calculate 
    |> printfn "%d"

"input" |> run 
