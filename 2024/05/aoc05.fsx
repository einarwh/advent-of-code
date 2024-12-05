// Advent of Code 2024. Day 05: Print Queue.
// dotnet fsi aoc05.fsx

open System
open System.IO

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parseRule s = 
    match s |> split "|" with 
    | [|a; b|] -> Some (int a, int b)
    | _ -> None

let parseUpdate s = 
    s |> split "," |> Array.toList |> List.map int 

let findRule rules page nextPage = 
    rules |> List.exists (fun (before, after) -> page = before && nextPage = after)

let checkPage rules pagesAfter page = 
    pagesAfter |> List.forall (findRule rules page)

let rec isSorted rules update = 
    match update with 
    | [] -> true 
    | page :: pagesAfter -> 
        checkPage rules pagesAfter page && isSorted rules pagesAfter

let middle lst = 
    let ix = (List.length lst) / 2 
    List.item ix lst

let part1 rules updates = 
    updates |> List.filter (isSorted rules) |> List.sumBy middle

let comparePages rules page1 page2 =
    if findRule rules page1 page2 then -1 else 1 

let part2 rules updates = 
    let unsorted = updates |> List.filter (fun update -> not <| isSorted rules update)
    let sorted = unsorted |> List.map (List.sortWith (comparePages rules))
    sorted |> List.sumBy middle

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let toLines = split "\n" >> Array.toList
    let rules = text.[0] |> toLines |> List.choose parseRule  
    let updates = text.[1] |> toLines |> List.map parseUpdate
    part1 rules updates |> printfn "%d"
    part2 rules updates |> printfn "%d"

run "input"
