// Advent of Code 2025. Day 12: Christmas Tree Farm.
// dotnet fsi aoc12.fsx

open System.IO

let parseRegionSize (s : string) = 
    match s.Split "x" with 
    | [|a; b|] -> (int a, int b) 
    | _ -> failwith <| sprintf "%s?" s

let parseNumbers (s : string) = 
    s.Split " " |> Array.toList |> List.map int 
    
let parseRegion (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> (parseRegionSize a, parseNumbers b)
    | _ -> failwith <| sprintf "'%s'?" s

let parseRegions (s : string) = 
    s.Trim().Split "\n" |> Array.toList |> List.map parseRegion
    
let parseCount (s : string) = 
    s.Split "\n" |> Array.toList |> List.tail |> List.sumBy (Seq.filter ((=) '#') >> Seq.length)

let isFeasible shapeSizes ((w, h), numbers) : bool =
    let minimum = List.zip shapeSizes numbers |> List.sumBy (fun (size, n) -> size * n)
    w * h >= minimum 

let isTrivial ((w, h), numbers) = 
    (w / 3) * (h / 3) >= List.sum numbers

let run fileName = 
    let text = File.ReadAllText fileName 
    let chunks = text.Split "\n\n" |> Array.toList |> List.rev 
    let regions = chunks |> List.head |> parseRegions 
    let shapeSizes = chunks |> List.tail |> List.rev |> List.map parseCount 
    let trivial = regions |> List.filter isTrivial 
    let feasible = regions |> List.filter (isFeasible shapeSizes)
    let nonTrivial = feasible |> List.except trivial 
    feasible |> List.length |> printfn "Feasible: %d" 
    trivial |> List.length |> printfn "Trivial: %d" 
    nonTrivial |> List.length |> printfn "Non-trivial: %d" 

run "input.txt"
