// Advent of Code 2025. Day 08: Playground.
// dotnet fsi aoc08.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList
    
let parse (s : string) = 
    match s.Split "," |> Array.map int with 
    | [|a; b; c|] -> (a, b, c)
    | _ -> failwith "?"

let distance (x1, y1, z1) (x2, y2, z2) = 
    let dx, dy, dz = x2-x1, y2-y1, z2-z1 
    let sq n = n * n |> float
    sqrt <| sq dx + sq dy + sq dz 
    
let getDistances boxes = 
    let rec loop acc boxes = 
        match boxes with 
        | a :: rest -> 
            let these = rest |> List.map (fun b -> (distance a b, (a, b))) 
            loop (these :: acc) rest 
        | _ -> 
            acc |> List.concat |> List.sortBy fst
    loop [] boxes
    
let connect conn circuits = 
    let rec loop acc circuits =
        printfn "\nloop..."
        circuits |> List.iter (printfn "%A")
        printfn "."
        match circuits with 
        | [] -> 
            printfn "found no match for %A, form new circuit" conn 
            (conn :: acc) |> List.rev 
        | circ :: rest -> 
            let shared = circ |> Set.intersect conn |> Set.count
            if shared > 0 then 
                let union = circ |> Set.union conn 
                printfn "overlap between %A and %A -> connect %A" conn circ union 
                List.rev (union :: acc) @ rest 
            else 
                printfn "no overlap between %A and %A" conn circ
                loop (circ :: acc) rest 
    loop [] circuits
    
let solve count boxes = 
    let rec loop circuits connections = 
        match connections with 
        | [] -> circuits 
        | conn :: rest ->
            loop (connect conn circuits) rest 
    let distances = boxes |> getDistances
    let toSet (a, b) = Set.empty |> Set.add a |> Set.add b 
    let connections = distances |> List.take count |> List.map (snd >> toSet)
    connections |> List.iter (printfn "%A")
    let result = loop [] connections
    printfn "%A" result
    0

let run fileName = 
    let lines = readLines fileName
    let boxes = lines |> List.map parse
    // boxes |> printfn "%A"
    // boxes |> getDistances |> printfn "%A"
    solve 10 boxes

run "sample.txt"
