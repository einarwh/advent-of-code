// Advent of Code 2020. Day 22: Crab Combat. Part A.
// dotnet fsi aoc22a.fsx

open System.IO

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parsePlayer (line : string) : int list = 
    match line |> trim |> split "\n" |> Array.toList with 
    | [] -> failwith "empty input"
    | _ :: t -> t |> List.map int 

let endGame winningCards : int = 
    winningCards |> List.rev |> List.mapi (fun i c -> (i + 1) * c) |> List.sum

let rec play (round : int) (cards1 : int list) (cards2 : int list) : int = 
    match (cards1, cards2) with 
    | [], _ -> 
        endGame cards2 
    | _, [] -> 
        endGame cards1
    | h1 :: t1, h2 :: t2 -> 
        let round' = round + 1
        if h1 > h2 then 
            play round' (t1 @ [h1; h2]) t2
        else 
            play round' t1 (t2 @ [h2; h1])

let run (cardArray : int list array) = 
    play 0 cardArray.[0] cardArray.[1] |> printfn "%d"

"input.txt"
|> File.ReadAllText
|> split "\n\n"
|> Array.map parsePlayer
|> run
