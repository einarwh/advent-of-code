// Advent of Code 2023. Day 12: Hot Springs
// dotnet fsi aoc12.fsx

open System
open System.IO

let parseLine (s : string) =
    let parts = s.Split(" ")
    let springs = parts[0]
    let damaged = parts[1]
    (springs |> Seq.toList, damaged.Split(",") |> Array.toList |> List.map int)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let rec consume springs damaged =
    if damaged = 0 then 
        match springs with
        | [] -> [ springs ]
        | '#' :: _ -> []
        | _ :: r -> [ r ]
    else
        match springs with
        | [] -> []
        | '.' :: _ -> []
        | _ :: rest ->
            consume rest (damaged - 1)

let rec findPossible springs damaged =
    if damaged = 0 then [ springs ]
    else
        match springs with
        | [] -> []
        | '.' :: rest ->
            findPossible rest damaged
        | '#' :: _ ->
            consume springs damaged
        | '?' :: rest ->
            let p1 = findPossible rest damaged // ? as '.'
            let p2 = consume springs damaged // ? as '#'
            p1 @ p2
        | _ -> failwith "oof"

let rec loop cache record =
    if Map.containsKey record cache then 
        (cache, cache[record])
    else 
        let (springs, pattern) = record
        match pattern with
        | [] -> 
            let res = if List.contains '#' springs then 0L else 1L 
            (cache |> Map.add record res, res)
        | dmg :: remaining ->
            let folder (cache, acc) possible = 
                let (cache, results) = loop cache (possible, remaining) 
                (cache, results + acc)
            let (cache, acc) = findPossible springs dmg |> List.fold folder (cache, 0) 
            (cache |> Map.add record acc, acc)

let solve record =
    loop Map.empty record |> snd

let unfold n (springs, pattern) = 
    let unfoldedSprings = 
        springs |> List.replicate n |> List.reduce (fun a b -> a @ ['?'] @ b) 
    let unfoldedPattern = 
        pattern |> List.replicate n |> List.concat 
    (unfoldedSprings, unfoldedPattern)

let run fileName =
    let lines = readLines fileName |> Array.toList
    lines
    |> List.sumBy (parseLine >> solve)
    |> printfn "%d"
    lines
    |> List.sumBy (parseLine >> unfold 5 >> solve)
    |> printfn "%d"

"input.txt" |> run
