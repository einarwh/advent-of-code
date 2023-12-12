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

let rec loopy (pattern : int list) (springs : char list) (cache : Map<int list * char list, int64>) : (Map<int list * char list, int64> * int64) =
    if Map.containsKey (pattern, springs) cache then 
        let cached = cache[(pattern, springs)]
        (cache, cached)
    else 
        let updatedCache, rs = 
            match pattern with
            | [] -> 
                let res = if List.contains '#' springs then 0L else 1L 
                (cache |> Map.add (pattern, springs) res, res)
            | dmg :: remaining ->
                let possibles : char list list = findPossible springs dmg
                let folder (cache : Map<int list * char list, int64>, acc) (possible : char list) = 
                    let (cache', results) = loopy remaining possible cache 
                    (cache', results + acc)
                let (cache, acc) = possibles |> List.fold folder (cache, 0) 
                (cache |> Map.add (pattern, springs) acc, acc)
        (updatedCache, rs)

let rec loop pattern (springs : char list) : int =
    match pattern with
    | [] ->
        if List.contains '#' springs then 0 else 1 
    | dmg :: remaining ->
        let possibles = findPossible springs dmg
        possibles 
        |> List.map (fun (possible : char list) -> loop remaining possible)
        |> List.sum 

let debugSolve i (springs : char list, pattern : int list) =
    let startTime = DateTime.Now 
    let (_, result) = loopy pattern springs Map.empty
    let elapsedSeconds = (DateTime.Now - startTime).TotalSeconds |> int 
    printfn "Line %d has %d permutations (%d seconds)" (i + 1) result elapsedSeconds
    result 

let solve (springs : char list, pattern : int list) =
    loopy pattern springs Map.empty |> snd

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
    |> List.sumBy solve
    |> printfn "%d"
    lines
    |> List.map parseLine
    |> List.map (unfold 5)
    |> List.sumBy solve
    |> printfn "%d"

"input" |> run
