// Advent of Code 2024. Day 23: LAN Party.
// Ref: https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
// dotnet fsi aoc23.fsx

open System
open System.IO

let parse (s : string) : string*string = 
    let parts = s.Split "-"
    let a = parts[0]
    let b = parts[1]
    if a < b then (a, b) else (b, a)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let tryFindCircle (map : Map<string, string list>) (c1 : string) =
    let tryFindThird (c2 : string) = 
        match Map.tryFind c2 map with 
        | Some cs -> cs |> List.map (fun c3 -> c1, c2, c3)
        | None -> []
    let tryFindConnection (c3 : string) = 
        let cs = Map.find c1 map
        cs |> List.contains c3
    let triplets = Map.find c1 map |> List.collect tryFindThird
    let circles = triplets |> List.choose (fun (c1, c2, c3) -> if tryFindConnection c3 then Some (c1, c2, c3) else None)
    circles

let rec bronKerbosch R P X graph =
    let neighbors vertex =
        graph
        |> List.find (fun (v, _) -> v = vertex)
        |> snd
        |> set
    seq {
        if (Set.isEmpty P) && (Set.isEmpty X) then
          yield (Set.toSeq R)
        let vPX =
            Seq.unfold
                (function
                | (v::tailP as P, currX) ->
                    let newX = Set.add v currX
                    Some((v, set <| P, currX), (tailP, newX))
                | ([], _) -> None)
                (P |> Set.toList, X)
        for (v, P, X) in vPX do
            let n = neighbors v
            yield! bronKerbosch (Set.add v R) (Set.intersect P n) (Set.intersect X n) graph
    }

type Graph = (string * (string list)) list

let findMaxCliques (graph : Graph) = bronKerbosch Set.empty (graph |> List.map fst |> set) Set.empty graph

let run fileName = 
    let lines = readLines fileName
    let connections = lines |> List.map parse |> List.sort
    let map = 
        connections 
        |> List.groupBy fst 
        |> List.sort 
        |> List.map (fun (c, cs) -> (c, cs |> List.map snd |> List.sort))
        |> Map.ofList 
    let keys = Map.keys map |> Seq.toList
    let circles = keys |> List.collect (tryFindCircle map)
    let t (c : string) = c.StartsWith("t")
    let ts = circles |> List.filter (fun (c1, c2, c3) -> t c1 || t c2 || t c3)
    ts |> List.length |> printfn "%d"
    let flipped = connections |> List.map (fun (a,b) -> (b, a))
    let graph = 
        connections @ flipped 
        |> List.groupBy fst 
        |> List.sort 
        |> List.map (fun (c, cs) -> (c, cs |> List.map snd |> List.sort))
    let wat = graph |> findMaxCliques |> Seq.maxBy (fun clique -> Seq.length clique) |> Seq.toList |> String.concat ","
    wat |> printfn "%s"

run "input.txt"
