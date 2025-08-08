// Advent of Code 2023. Day 21: Step Counter. Part 1.
// dotnet fsi aoc21.fsx

open System
open System.IO

let parseLine (pred : char -> bool) (y : int) = 
    Seq.toList 
    >> List.mapi (fun x ch -> if pred ch then Some (x, y) else None)
    >> List.choose id

let getNeighbours (x, y) = 
    [ (x, y - 1) 
      (x - 1, y)
      (x + 1, y)
      (x, y + 1) ]

let fill start steps space = 
    let rec loop queue visited = 
        match queue with 
        | [] -> 
            visited 
            |> Set.toList 
            |> List.choose (fun (pos, steps) -> if steps = 0 then Some pos else None)
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
    loop [ (start, steps) ] (Set.empty)

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let run fileName =
    let lines = readLines fileName |> Array.toList
    let parse p = List.mapi (parseLine p) >> List.concat 
    let space = lines |> parse ((<>) '#') |> Set.ofList
    let start = lines |> parse ((=) 'S') |> List.head
    let steps = 64
    let result = fill start steps space 
    result |> List.length |> printfn "%d" 

"input.txt" |> run
