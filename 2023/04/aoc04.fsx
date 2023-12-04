// Advent of Code 2023. Day 4: Scratchcards
// dotnet fsi aoc04.fsx

open System
open System.IO

let split (sep : string) (s : string) = s.Split(sep)

let substring from (s : string) = s.Substring(from)

let isNonEmpty (s : string) = s.Length > 0  

let parseCard (line : string) : int = 
    let s1 = split ": " line 
    let cardNo = s1[0] |> substring ("Card ".Length) |> int
    let s2 = split " | " s1[1]
    let winning = s2[0] |> split " " |> Array.filter isNonEmpty |> Array.map int 
    let numbers = s2[1] |> split " " |> Array.filter isNonEmpty |> Array.map int  
    Set.intersect (Set.ofArray winning) (Set.ofArray numbers) |> Set.count

let calculatePoints (wins : int) : int = 
    match wins with 
    | 0 -> 0 
    | n -> pown 2 (n - 1)

let listCards (cardLimit : int) (cardIndex : int) (cardsWon : int) = 
    if cardsWon = 0 then []
    else 
        let startIndex = cardIndex + 1
        if startIndex < cardLimit then 
            let endIndex = min (cardIndex + cardsWon) cardLimit
            [startIndex .. endIndex]
        else []

let rec clone count lst result = 
    if count = 0 then result 
    else clone (count - 1) lst (lst @ result)

let rec calculateCards (cardIndex : int) (carry : int list) (winnings : int list list) = 
    match winnings with 
    | [] -> [] 
    | current :: t -> 
        let (matches, rest) = carry |> List.partition (fun x -> x = cardIndex)
        let matchCount = matches |> List.length 
        let cloned = clone matchCount current current
        let carry' = rest @ cloned
        (1 + matchCount) :: calculateCards (cardIndex + 1) carry' t

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let winsList = lines |> List.map parseCard
    winsList 
    |> List.sumBy calculatePoints 
    |> printfn "%d"
    let limit = winsList |> List.length
    winsList 
    |> List.mapi (listCards limit)
    |> calculateCards 0 [] 
    |> List.sum 
    |> printfn "%d"

"input" |> run 
