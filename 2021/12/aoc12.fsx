// Advent of Code 2021. Day 12: Passage Pathing
// dotnet fsi aoc12.fsx

open System
open System.IO

let parseLine (s : string) = 
    match s.Split('-') with 
    | [| a; b |] -> (a, b)
    | _ -> failwith <| sprintf "%s?" s

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let solve1 (map : Map<string, string list>) = 
    let rec loop (seen : Set<string>) (current : string) = 
        match current with 
        | "end" -> 1 
        | _ -> 
            if Set.contains current seen then 0 
            else 
                let seen = 
                    if Char.IsUpper(current[0]) then seen 
                    else seen |> Set.add current 
                let possibilities = Map.find current map
                possibilities |> List.sumBy (loop seen) 
    loop Set.empty "start"

let solve2 (map : Map<string, string list>) = 
    let rec loop (seen : Set<string>) (usedExtraVisit : bool) (current : string) = 
        match current with 
        | "end" -> 1 
        | _ -> 
            if (Set.contains current seen && (usedExtraVisit || current = "start")) then 0 
            else 
                let usedExtraVisit = usedExtraVisit || Set.contains current seen
                let seen = 
                    if Char.IsUpper(current[0]) then seen 
                    else seen |> Set.add current 
                let possibilities = Map.find current map
                possibilities |> List.sumBy (loop seen usedExtraVisit) 
    loop Set.empty false "start"

let run fileName =
    let lines = readLines fileName |> Array.toList
    let parsed = lines |> List.map parseLine
    let swap (a, b) = (b, a)
    let edges = parsed @ (parsed |> List.map swap)
    edges 
    |> List.groupBy fst 
    |> List.map (fun (n, es) -> (n, es |> List.map snd))
    |> Map.ofList 
    |> solve1
    |> printfn "%d"
    edges 
    |> List.groupBy fst 
    |> List.map (fun (n, es) -> (n, es |> List.map snd))
    |> Map.ofList 
    |> solve2
    |> printfn "%d"

"input" |> run
