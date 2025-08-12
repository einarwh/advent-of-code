// Advent of Code 2020. Day 23: Crab Cups.
// dotnet fsi aoc23.fsx

open System
open System.IO

let findDestination (cups : int list) (maxCup : int) (curr : int) = 
    let rec loop i = 
        if i < 1 then loop maxCup 
        else if List.contains i cups then loop (i - 1)
        else i 
    loop (curr - 1)

let move (maxCup : int) (links : Map<int, int>, curr : int) = 
    let cup1 = links[curr]
    let cup2 = links[cup1]
    let cup3 = links[cup2]
    let next = links[cup3]
    let cups = [cup1; cup2; cup3]
    let dest = findDestination cups maxCup curr 
    let links' = 
        links 
        |> Map.add curr next 
        |> Map.add dest cup1 
        |> Map.add cup3 links[dest]
    (links', next)

let toCups (links : Map<int,int>) = 
    let rec loop acc curr = 
        match links[curr] with 
        | 1 -> List.rev acc 
        | c -> loop (c :: acc) c 
    loop [] 1 |> List.toArray

let play moves (cups : int array) = 
    // printfn "cups: %A" cups
    let maxCup = cups |> Array.max 
    let rec loop i (links, curr) = 
        if i <= moves then 
            loop (i + 1) (move maxCup (links, curr))
        else 
            (links, curr)
    let links = 
        cups 
        |> Array.toList 
        |> List.indexed 
        |> List.fold (fun ls (ix, c) -> ls |> Map.add c (cups[(ix + 1) % cups.Length])) Map.empty
    // printfn "%A" links
    loop 1 (links, cups[0]) |> fst |> toCups

let part1 cups = 
    cups |> play 100 |> Array.map string |> String.concat "" |> printfn "%s"

let part2 cups = 
    let moreCups = Array.concat [|cups; [|10 .. 1000000|]|]
    printfn "%A" (Array.max moreCups)
    printfn "%A" (Array.take 20 moreCups)
    let order = play 10000000 moreCups 
    printfn "%A" (Array.take 20 order)
    int64 order[0] * int64 order[1] |> printfn "%d"

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    let cups = text |> Seq.toArray |> Array.map (fun ch -> Int32.Parse(ch.ToString()))
    cups |> part1 
    cups |> part2
    0

run "input.txt"
