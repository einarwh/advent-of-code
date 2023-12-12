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

let rec arrangements (springs : char list) : char list list=
    match springs with
    | [] -> [[]]
    | s :: t ->
        let rest = arrangements t
        match s with
        | '?' ->
            let broken = rest |> List.map (fun r -> '#' :: r)
            let working = rest |> List.map (fun r -> '.' :: r)
            broken @ working
        | _ ->
            rest |> List.map (fun r -> s :: r)

let countDamaged (s : string) =
    Regex.Matches(s, "\#+")
    |> Seq.map (fun m -> m.Value |> String.length)
    |> Seq.toList

let check (pattern : int list) (springs : string) =
    let damaged = countDamaged springs
    if List.length damaged = List.length pattern then
        List.zip damaged pattern
        |> List.forall (fun (a, b) -> a = b)
    else false

let rec tryConsume (springs : char list) (damaged : int) =
    if damaged = 0 then
        printfn "done consuming: %s" (toStr springs)
        Some springs
    else
        match springs with
        | [] -> None
        | '.' :: _ -> None
        | '#' :: rest ->
            printfn "consuming #"
            tryConsume rest (damaged - 1)
        | '?' :: rest ->
            printfn "consuming ? as #"
            tryConsume rest (damaged - 1)
        | _ -> failwith "no"


let consume (springs : char list) (damaged : int) =
    printfn "consume %d damaged from %s" damaged (toStr springs)
    match tryConsume springs damaged with
    | Some s ->
        match s with
        | [] ->
            printfn "consumed %d: %s -> %s" damaged (toStr springs) (toStr s)
            [ s ]
        | '#' :: _ -> []
        | _ :: r ->
            printfn "consumed %d + 1: %s -> %s" damaged (toStr springs) (toStr r)
            [ r ]
    | None -> []

let rec findPossible (springs : char list) (damaged : int) =
    printfn "find possible %s" (toStr springs)
    if damaged = 0 then [ springs ]
    else
        match springs with
        | [] -> []
        | '.' :: rest ->
            printfn "skip dot"
            findPossible rest damaged
        | '#' :: _ ->
            printfn "# -> try consume"
            match tryConsume springs damaged with
            | Some s ->
                match s with
                | [] -> [ s ]
                | '#' :: _ -> []
                | _ :: r -> [ r ]
            | None -> []
        | '?' :: rest ->
            printfn "skip ? as ."
            let p1 = findPossible rest damaged // ? as '.'
            printfn "..."
            printfn "consume ? as #"
            let p2 = consume springs damaged // ? as '#'
            let combined = p1 @ p2
            printfn "got %A" combined
            combined
        | _ -> failwith "oof"

let count (springs : char list, pattern : int list) =
    let damaged = pattern |> List.head
    printfn "damaged = %d" damaged
    printfn "springs = %s" (toStr springs)
    let possibleSprings = findPossible springs damaged
    printfn "possible = %A" possibleSprings
    []

let rec loop pattern springs =
    match pattern with
    | [] -> [ springs ]
    | dmg :: remaining ->
        let possibles = findPossible springs dmg
        possibles |> List.collect (fun (possible : char list) -> loop remaining possible)

let solve (springs : char list, pattern : int list) =
    let foo = loop pattern springs
    let bar = 
        foo 
        |> List.filter (fun leftover -> List.contains '#' leftover |> not) 
    printfn "%A" bar
    bar |> List.length

let countArrangements (line : string) =
    let springs, damaged = line |> parseLine
    springs
    |> Seq.toList
    |> arrangements
    |> List.map (List.toArray >> String)
    |> List.filter (check damaged)
    |> List.length

let run fileName =
    let lines = readLines fileName |> Array.toList
    lines
    |> List.map countArrangements
    |> List.sum
    |> printfn "%d"
    let s1 = "????.######..#####. 1,6,5"
    let s2 = "?###???????? 3,2,1"
    // let foo = count (parseLine s2)
    let bar = solve (parseLine s2)
    lines
    |> List.map parseLine
    |> List.map solve
    |> List.sum
    |> printfn "%A"

    printfn "."




"sample" |> run
