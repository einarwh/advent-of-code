// Advent of Code 2017. Day 12: Digital Plumber.
// dotnet fsi aoc12.fsx

open System
open System.IO

let contains n = List.exists (fun m -> n = m)
  
let rec group lookup members key = 
    if contains key members then members
    else  
        let members' = key :: members
        lookup key 
        |> List.filter (not << fun t -> contains t members')
        |> List.fold (group lookup) members'
  
let makeLookup vals = 
    let map = Map.ofList vals
    fun it -> Map.find it map 

let solve1 vals = 
    group (makeLookup vals) [] 0 |> List.length

let solve2 vals = 
    let lookup = makeLookup vals
    let rec solve all gs = function 
    | [] -> gs
    | (n, _) :: t ->
        if contains n all then 
            solve all gs t
        else     
            let g = group lookup [] n
            solve (g @ all) (g :: gs) t
    solve [] [] vals |> List.length

let split (splitter : string) (s : string) = 
    s.Split([|splitter|], StringSplitOptions.None)

let parseLine (line : string) =
    match line |> split " <-> " with 
    | [|keyStr; valuesStr|] ->  
        let key = int keyStr
        let values = valuesStr |> split ", " |> Array.map int |> Array.toList
        Some (key, values)
    | _ -> None

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let values = lines |> List.choose parseLine 
    values |> solve1 |> printfn "%d"
    values |> solve2 |> printfn "%A"

run "input.txt"
