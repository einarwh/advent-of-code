// Advent of Code 2022. 
// Day 13: Distress Signal.
// dotnet fsi aoc13.fsx

open System
open System.IO

type Item = 
    | N of int
    | L of Item list 

type Compared = 
    | LT | EQ | GT

let split (splitter : string) (s : string) = 
    s.Split(splitter)

let trim (s : string) = 
    s.Trim()

let tryRead ix s = 
    if ix < String.length s then 
        Some s.[ix]
    else 
        None

let rec parseItem  ix s = 
    match tryRead ix s with
    | Some '[' -> 
        parseList ix s 
    | Some _ -> 
        parseNumber ix s 
    | None -> failwith "Nothing to read"
and parseNumber ix s = 
    let toNumber acc = 
        acc |> List.rev |> List.toArray |> String |> int |> N
    let rec fn acc ix s = 
        match tryRead ix s with 
        | Some ch when Char.IsDigit ch -> 
            fn (ch :: acc) (ix + 1) s 
        | _ -> 
            (toNumber acc, ix)
    fn [] ix s 
and parseList ix s = 
    let rec fn items ix s = 
        let (item, ix) = parseItem ix s 
        match tryRead ix s with 
        | Some ']' -> 
            (item :: items |> List.rev |> L, ix + 1)
        | Some ',' -> 
            fn  (item :: items) (ix + 1) s
        | _ -> 
            failwith "failed to parse list"
    match tryRead (ix + 1) s with 
    | Some ']' -> (L [], ix + 2)
    | _ -> fn [] (ix + 1) s

let parseLine (s : string) = 
    parseItem 0 s |> fst

let parsePair (s : string) = 
    match s |> split "\n" with 
    | [|a; b|] -> (parseLine a, parseLine b) 
    | _ -> failwith <| sprintf "Unexpected input %s" s 

let rec compareItems (item1 : Item, item2 : Item) : Compared = 
    match (item1, item2) with 
    | (N n1, N n2) -> 
        if n1 < n2 then LT 
        else if n1 = n2 then EQ 
        else GT
    | (L lst1, L lst2) -> 
        compareLists (lst1, lst2)
    | (N n, L lst) -> 
        compareItems (L [ N n ], L lst)
    | (L lst, N n) -> 
        compareItems (L lst, L [ N n ])
and compareLists (lst1 : Item list, lst2 : Item list) = 
    match (lst1, lst2) with 
    | ([], []) -> EQ
    | ([], _) -> LT 
    | (_, []) -> GT 
    | (h1 :: t1, h2 :: t2) -> 
        match compareItems (h1, h2) with 
        | LT -> LT 
        | GT -> GT 
        | EQ -> compareLists (t1, t2)

let part1 lines = 
    lines 
    |> List.map parsePair 
    |> List.mapi (fun ix pair -> if LT = compareItems pair then Some (ix+1) else None) 
    |> List.choose id
    |> List.sum
    |> printfn "Sum: %d"

let sort (lines : Item list) : Item list = 
    let rec insert (seen : Item list) (sorted : Item list) (item : Item) = 
        match sorted with 
        | [] -> (item :: seen) |> List.rev 
        | h :: t -> 
            match compareItems (item, h) with 
            | LT -> (List.rev seen) @ (item :: sorted)
            | _ -> insert (h :: seen) t item 
    let rec fn (sorted : Item list) (lines : Item list) = 
        match lines with 
        | [] -> sorted 
        | h :: t -> 
            fn (insert [] sorted h) t
    fn [] lines

let findPacketIndex (items : Item list) (packet : Item) = 
    1 + List.findIndex (fun it -> it = packet) items

let findPacketIndexes (packets : Item list) (items : Item list) = 
    packets |> List.map (findPacketIndex items)

let part2 lines = 
    let (p1, _) = "[[2]]" |> parseItem 0
    let (p2, _) = "[[6]]" |> parseItem 0
    lines 
    |> List.collect (split "\n" >> Array.toList)
    |> List.map (parseItem 0 >> fst)
    |> (@) [p1;p2]
    |> sort
    |> findPacketIndexes [p1;p2]
    |> List.fold (*) 1
    |> printfn "Decoder key: %d" 

let run (lines : string list) =
    part1 lines
    part2 lines

"input.txt"
|> File.ReadAllText
|> trim 
|> split "\n\n"
|> Array.toList
|> run 
