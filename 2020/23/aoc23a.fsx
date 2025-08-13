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

let move (maxCup : int) (cups : int list, links : Map<int, int>, curr : int) = 
    printfn "cups %A" cups
    let cup1 = links[curr]
    let cup2 = links[cup1]
    let cup3 = links[cup2]
    let next = links[cup3]
    let removed = [cup1; cup2; cup3]
    printfn "pick up %A" removed
    let remaining = cups |> List.filter (fun c -> not <| List.contains c removed)
    printfn "remaining: %A" remaining
    let dest = findDestination removed maxCup curr 
    printfn "destination %d" dest
    let destIndex = List.findIndex (fun c -> c = dest) remaining
    let splitIndex = destIndex + 1
    printfn "splitIndex: %d" splitIndex
    let cups' = 
        if splitIndex < List.length cups then 
            let left, right = List.splitAt splitIndex remaining
            List.concat [left; removed; right]
        else 
            List.concat [remaining; removed]
    let links' = 
        links 
        |> Map.add curr next 
        |> Map.add dest cup1 
        |> Map.add cup3 links[dest]
    (cups', links', next)

let toCups (links : Map<int,int>) = 
    let rec loop acc curr = 
        match links[curr] with 
        | 1 -> List.rev acc 
        | c -> loop (c :: acc) c 
    loop [] 1 

let play moves (cups : int list) = 
    let maxCup = cups |> List.max 
    let rec loop i (cups, links, curr) = 
        if i <= moves then 
            printfn "\n-- move %d --" i
            loop (i + 1) (move maxCup (cups, links, curr))
        else 
            (cups, links, curr)
    let links = 
        cups 
        |> List.indexed 
        |> List.fold (fun ls (ix, c) -> ls |> Map.add c (cups[(ix + 1) % cups.Length])) Map.empty
    loop 1 (cups, links, cups[0]) |> fun (_, ls, _) -> ls 

let part1 cups = 
    cups |> play 100 |> toCups |> List.map string |> String.concat "" |> printfn "%s"

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    let cups = text |> Seq.toList |> List.map (fun ch -> Int32.Parse(ch.ToString()))
    cups |> part1 

run "sample.txt"
