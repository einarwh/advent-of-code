// Advent of Code 2020. Day 9.
// dotnet fsi aoc09.fsx

open System.IO

let toValues : string array -> int64 list = 
    Array.toList >> List.filter (fun (s : string) -> s.Length > 0) >> List.map int64

let findPairs (preceding : int64 list) (num : int64) : (int64 * int64) list =
    [ for i in preceding do
          for j in preceding do
              if j > i && i + j = num then yield (i, j) ]

let invalid preceding : int64 -> bool =
    findPairs preceding >> List.isEmpty
    
let rec findFirstInvalid (preceding : int64 list) (numbers : int64 list) : int64 option =
    match numbers with
    | [] -> None
    | n :: rest ->
        if invalid preceding n then
            Some n
        else 
            findFirstInvalid ((List.tail preceding) @ [n]) rest

let weakness (range : int64 list) =
    List.min range + List.max range

let rec findWeaknessAt (numbers : int64 list) (range : int64 list) (sum : int64) (num : int64) : int64 option =
    match numbers with
    | [] -> None
    | h :: t ->
        let sum' = sum + h
        if sum' > num then
            None
        else
            let range' = h :: range
            if (sum' = num) then
                Some <| weakness range'
            else
                findWeaknessAt t range' sum' num 

let rec findWeakness (numbers : int64 list) (num : int64) : int64 option =
    match numbers with
    | [] -> None
    | _ :: rest ->
        findWeaknessAt numbers [] 0L num
        |> Option.orElse (findWeakness rest num)
    
let run lines =
    let values = lines |> toValues
    let (preamble, numbers) = values |> List.splitAt 25
    match findFirstInvalid preamble numbers with
    | None -> printfn "No invalid number found?"
    | Some firstInvalid ->
        firstInvalid |> printfn "%d"
        match findWeakness values firstInvalid with
        | None -> printfn "No weakness found?"
        | Some weakness ->
            weakness |> printfn "%d" 

"input" |> File.ReadAllLines |> run 