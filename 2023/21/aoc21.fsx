// Advent of Code 2023. Day 20: Pulse Propagation.
// dotnet fsi aoc20.fsx

open System
open System.IO

let parseSpace (y : int) (s : string) = 
    s |> Seq.toList 
      |> List.mapi (fun x ch -> if ch <> '#' then Some (x, y) else None)
      |> List.choose id

let parseStart (y : int) (s : string) = 
    s |> Seq.toList 
      |> List.mapi (fun x ch -> if ch = 'S' then Some (x, y) else None)
      |> List.choose id

let getNeighbours (x, y) = 
    [ (x, y - 1) 
      (x - 1, y)
      (x + 1, y)
      (x, y + 1) ]

let fill (start : int * int) (steps : int) (space : Set<int*int>) = 
    let rec loop (queue : ((int*int) * int) list) (visited : Set<(int*int) * int>)= 
        match queue with 
        | [] -> visited 
        | ((x, y), steps) :: rest -> 
            if Set.contains ((x, y), steps) visited then
                loop rest visited 
            else 
                let visited = visited |> Set.add ((x, y), steps)
                if steps = 0 then 
                    loop rest visited 
                else 
                    let neighbours = 
                        (x, y) 
                        |> getNeighbours 
                        |> List.filter (fun p -> Set.contains p space)
                        |> List.map (fun p -> (p, steps - 1))
                    let queue = queue @ neighbours
                    loop queue visited
    let visited = loop [ (start, steps) ] (Set.empty)
    visited 
    |> Set.toList 
    |> List.choose (fun (pos, steps) -> if steps = 0 then Some pos else None)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName |> Array.toList
    let space = lines |> List.mapi parseSpace |> List.concat |> Set.ofList
    let start = lines |> List.mapi parseStart |> List.concat |> List.head
    let steps = 64
    let result = fill start steps space 
    result |> List.length |> printfn "%d" 

"input" |> run