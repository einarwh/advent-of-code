// Advent of Code 2023. Day 12: Hot Springs
// dotnet fsi aoc12.fsx

open System
open System.IO

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
        Some springs
    else
        match springs with
        | [] -> None
        | '.' :: _ -> None
        | '#' :: rest ->
            tryConsume rest (damaged - 1)
        | '?' :: rest ->
            tryConsume rest (damaged - 1)
        | _ -> failwith "no"

let consume (springs : char list) (damaged : int) =
    match tryConsume springs damaged with
    | Some s ->
        match s with
        | [] -> [ s ]
        | '#' :: _ -> []
        | _ :: r -> [ r ]
    | None -> []

let rec findPossible (springs : char list) (damaged : int) =
    if damaged = 0 then [ springs ]
    else
        match springs with
        | [] -> []
        | '.' :: rest ->
            findPossible rest damaged
        | '#' :: _ ->
            match tryConsume springs damaged with
            | Some s ->
                match s with
                | [] -> [ s ]
                | '#' :: _ -> []
                | _ :: r -> [ r ]
            | None -> []
        | '?' :: rest ->
            let p1 = findPossible rest damaged // ? as '.'
            let p2 = consume springs damaged // ? as '#'
            p1 @ p2
        | _ -> failwith "oof"

let rec loop (pattern : int list) (springs : char list) (cache : Map<int list * char list, char list list>) : (Map<int list * char list, char list list> * char list list) =
    if Map.containsKey (pattern, springs) cache then 
        let cached = cache[(pattern, springs)]
        (cache, cached)
    else 
        let updatedCache, rs = 
            match pattern with
            | [] -> (cache |> Map.add (pattern, springs) [ springs ], [ springs ])
            | dmg :: remaining ->
                let possibles : char list list = findPossible springs dmg
                let folder (cache : Map<int list * char list, char list list>, acc) (possible : char list) = 
                    let (cache', results) = loop remaining possible cache 
                    (cache', results @ acc)
                let (cache, acc) = possibles |> List.fold folder (cache, []) 
                cache, acc 
        (updatedCache, rs)

let debugSolve i (springs : char list, pattern : int list) =
    let startTime = DateTime.Now 
    let cache = Map.empty
    let (_, results) = loop pattern springs cache
    let result = 
        results
        |> List.filter (fun leftover -> List.contains '#' leftover |> not) 
        |> List.length
    let elapsedSeconds = (DateTime.Now - startTime).TotalSeconds |> int 
    printfn "Line %d has %d permutations (%d seconds)" (i + 1) result elapsedSeconds
    result 

let solve (springs : char list, pattern : int list) =
    let (_, results) = loop pattern springs Map.empty    
    results
    |> List.filter (fun leftover -> List.contains '#' leftover |> not) 
    |> List.length

let unfold n (springs, pattern) = 
    let unfoldedSprings = 
        [0 .. n - 1] |> List.map (fun _ -> springs) |> List.reduce (fun a b -> a @ ['?'] @ b) 
    let unfoldedPattern = 
        [0 .. n - 1] |> List.collect (fun _ -> pattern)
    (unfoldedSprings, unfoldedPattern)

let run fileName =
    let lines = readLines fileName |> Array.toList
    lines
    |> List.map parseLine
    |> List.map solve
    |> List.sum
    |> printfn "%A"
    lines
    |> List.map parseLine
    |> List.map (unfold 5)
    |> List.mapi debugSolve
    |> List.sum
    |> printfn "%A"

"input" |> run
