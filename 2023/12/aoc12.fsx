// Advent of Code 2023. Day 12: Hot Springs
// dotnet fsi aoc12.fsx

open System
open System.IO
open System.Text.RegularExpressions

let toStr = List.toArray >> String

let parseLine (s : string) =
    let parts = s.Split(" ")
    let springs = parts[0]
    let damaged = parts[1]
    (springs |> Seq.toList, damaged.Split(",") |> Array.toList |> List.map int)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let rec tryConsume (springs : char list) (damaged : int) =
    if damaged = 0 then
        // printfn "done consuming: %s" (toStr springs)
        Some springs
    else
        match springs with
        | [] -> None
        | '.' :: _ -> None
        | '#' :: rest ->
            // printfn "consuming #"
            tryConsume rest (damaged - 1)
        | '?' :: rest ->
            // printfn "consuming ? as #"
            tryConsume rest (damaged - 1)
        | _ -> failwith "no"

let consume (springs : char list) (damaged : int) =
    // printfn "consume %d damaged from %s" damaged (toStr springs)
    match tryConsume springs damaged with
    | Some s ->
        match s with
        | [] ->
            // printfn "consumed %d: %s -> %s" damaged (toStr springs) (toStr s)
            [ s ]
        | '#' :: _ -> []
        | _ :: r ->
            // printfn "consumed %d + 1: %s -> %s" damaged (toStr springs) (toStr r)
            [ r ]
    | None -> []

let rec findPossible (springs : char list) (damaged : int) =
    // printfn "find possible %s" (toStr springs)
    if damaged = 0 then [ springs ]
    else
        match springs with
        | [] -> []
        | '.' :: rest ->
            // printfn ". -> skip"
            findPossible rest damaged
        | '#' :: _ ->
            // printfn "# -> try consume"
            match tryConsume springs damaged with
            | Some s ->
                match s with
                | [] -> [ s ]
                | '#' :: _ -> []
                | _ :: r -> [ r ]
            | None -> []
        | '?' :: rest ->
            // printfn "skip ? as ."
            let p1 = findPossible rest damaged // ? as '.'
            // printfn "..."
            // printfn "consume ? as #"
            let p2 = consume springs damaged // ? as '#'
            let combined = p1 @ p2
            // printfn "got %A" combined
            combined
        | _ -> failwith "oof"

let rec loop pattern springs =
    match pattern with
    | [] -> [ springs ]
    | dmg :: remaining ->
        let possibles = findPossible springs dmg
        possibles |> List.collect (fun (possible : char list) -> loop remaining possible)

let solve i (springs : char list, pattern : int list) =
    printfn "solve %d" i 
    let startTime = DateTime.Now 
    let result = 
        loop pattern springs
        |> List.filter (fun leftover -> List.contains '#' leftover |> not) 
        |> List.length
    let elapsedSeconds = (DateTime.Now - startTime).TotalSeconds |> int 
    printfn "...%d seconds" elapsedSeconds
    result 

let unfold n (springs, pattern) = 
    let unfoldedSprings = 
        [0 .. n - 1] |> List.map (fun _ -> springs) |> List.reduce (fun a b -> a @ ['?'] @ b) 
    let unfoldedPattern = 
        [0 .. n - 1] |> List.collect (fun _ -> pattern)
    // printfn "%s" (toStr unfoldedSprings)
    // printfn "%A" unfoldedPattern
    (unfoldedSprings, unfoldedPattern)

let run fileName =
    let lines = readLines fileName |> Array.toList
    let s1 = "????.######..#####. 1,6,5"
    let s2 = "?###???????? 3,2,1"
    lines
    |> List.map parseLine
    |> List.map (unfold 5)
    |> List.mapi solve
    |> List.sum
    |> printfn "%A"

"input" |> run
