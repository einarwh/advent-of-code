// Advent of Code 2021. Day 14: Extended Polymerization.
// dotnet fsi aoc14.fsx

open System
open System.IO

type Rule = char*char -> char list

type ComposableRule = Rule -> Rule 

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parseRule (s : string) : ComposableRule = 
    match s |> split " -> " with 
    | [|pair; element|] ->
        let (x, y) = (pair.[0], pair.[1])
        let e = element.[0]
        fun fn (a, b) -> if (a, b) = (x, y) then [a; e; b] else fn (a, b)
    | _ -> failwith "?"

let createInsertionFunction (s : string) = 
    let rules : ComposableRule list = s |> split "\n" |> Array.toList |> List.map parseRule
    // 'T -> 'State -> 'State
    let folder (rule : ComposableRule) (fn : Rule) : Rule = 
        rule fn 
    let initFunction (a, b) = [a; b]
    List.foldBack folder rules initFunction

let rec insert insertionFunction pairs = 
    match pairs with 
    | [] -> [] 
    | p :: rest -> 
        (insertionFunction p) :: (insert insertionFunction rest)

let join lst = 
    match lst with 
    | [] -> failwith "?"
    | h :: rest -> 
        h @ (rest |> List.collect (List.tail))
    
let rec loop inserter times elements = 
    printfn "%A" elements 
    if times < 1 then elements 
    else 
        elements 
        |> List.pairwise 
        |> insert inserter 
        |> join 
        |> loop inserter (times - 1)

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let template = text.[0]
    let insertionFunction = createInsertionFunction text.[1]
    let pairs = template |> Seq.toList |> List.pairwise 
    printfn "%A" pairs
    pairs |> insert insertionFunction |> join |> printfn "%A"

    let polymer10 = template |> Seq.toList |> loop insertionFunction 10 
    

    0

run "sample"

// ['N'; 'C'; 'N'; 'B'; 'C'; 'H'; 'B']
// NCNBCHB
