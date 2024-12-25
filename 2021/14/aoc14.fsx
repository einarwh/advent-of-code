// Advent of Code 2021. Day 14: Extended Polymerization.
// dotnet fsi aoc14.fsx

open System
open System.IO

type Pair = char * char

type Rule = Pair -> Pair list

type ComposableRule = Rule -> Rule 

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parseRule (s : string) : ComposableRule = 
    match s |> split " -> " with 
    | [|pair; element|] ->
        let (x, y) = (pair.[0], pair.[1])
        let e = element.[0]
        fun fn (a, b) -> if (a, b) = (x, y) then [(a, e); (e, b)] else fn (a, b)
    | _ -> failwith "?"

let createInsertionFunction (s : string) : Rule = 
    let rules : ComposableRule list = s |> split "\n" |> Array.toList |> List.map parseRule
    // 'T -> 'State -> 'State
    let folder (rule : ComposableRule) (fn : Rule) : Rule = 
        rule fn 
    let initFunction (a, b) = [(a, b)]
    List.foldBack folder rules initFunction

let rec insert insertionFunction pairs = 
    match pairs with 
    | [] -> [] 
    | p :: rest -> 
        (insertionFunction p) :: (insert insertionFunction rest)

let polymerization (inserter : Rule) (countedPairs : (Pair * int64) list) : (Pair * int64) list = 
    let rec loop (countedPairs : (Pair * int64) list) : (Pair * int64) list = 
        match countedPairs with 
        | [] -> []
        | (pair, count) :: rest -> 
            let lst = pair |> inserter |> List.map (fun p -> (p, count))
            lst @ loop rest
    let cps = loop countedPairs
    cps |> List.groupBy (fst) |> List.map (fun (p, lst) -> (p, lst |> List.map snd |> List.sum))

let rec solve (times : int) (inserter : Rule) (countedPairs : (Pair * int64) list) = 
    let rec loop times countedPairs = 
        if times > 0 then 
            loop (times - 1) (polymerization inserter countedPairs)
        else 
            countedPairs
    let result = loop times countedPairs
    let fstList = result |> List.groupBy (fst >> fst)
    let sndList = result |> List.groupBy (fst >> snd)
    let sum lst = lst |> List.map (fun (ch, lst) -> (ch, lst |> List.map snd |> List.sum))
    let lst = 
        List.zip (fstList |> sum |> List.sort) (sndList |> sum |> List.sort) 
        |> List.map (fun ((ch1, n1), (ch2, n2)) -> max n1 n2)
    let minVal = lst |> List.min
    let maxVal = lst |> List.max 
    maxVal - minVal 

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n\n"
    let template : string = text.[0]
    let inserter = createInsertionFunction text.[1]
    let countedPairs = 
        template 
        |> Seq.toList 
        |> List.pairwise 
        |> List.countBy id 
        |> List.map (fun (p, c) -> (p, int64 c))
    countedPairs
    |> solve 10 inserter 
    |> printfn "%d"
    countedPairs
    |> solve 40 inserter 
    |> printfn "%d"
    0

run "input"
