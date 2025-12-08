// Advent of Code 2025. Day 08: Playground.
// dotnet fsi aoc08.fsx

open System
open System.IO

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList
    
let parse (s : string) = 
    match s.Split "," |> Array.map int64 with 
    | [|a; b; c|] -> (a, b, c)
    | _ -> failwith "?"

let distance (x1, y1, z1) (x2, y2, z2) = 
    let dx, dy, dz = x2-x1, y2-y1, z2-z1 
    let sq n = double <| n * n
    sqrt <| sq dx + sq dy + sq dz 
    
let getDistances boxes = 
    let rec loop acc boxes = 
        match boxes with 
        | a :: rest -> 
            let distances = rest |> List.map (fun b -> distance a b, (a, b)) 
            loop (distances :: acc) rest 
        | _ -> 
            acc |> List.concat |> List.sortBy fst
    loop [] boxes

let useConnection (box1, box2) circuits = 
    let contains box = List.filter (Set.contains box)
    let included = contains box1 circuits @ contains box2 circuits
    let excluded = circuits |> List.except included
    let connected = included |> List.reduce Set.union
    connected :: excluded 

let rec connect connections circuits = 
    match connections with 
    | [] -> circuits  
    | conn :: rest -> circuits |> useConnection conn |> connect rest 
    
let rec connectUntilOne connections circuits = 
    match connections with 
    | [] -> None   
    | conn :: rest -> 
        let circuits' = useConnection conn circuits 
        if List.length circuits' = 1 then Some conn else connectUntilOne rest circuits' 

let singleton item = Set.empty |> Set.add item 

let solveA connections = 
    let circuits = connections |> List.collect (fun (a, b) -> [a; b]) |> List.distinct |> List.map singleton 
    let sizes = circuits |> connect connections |> List.map Set.count |> List.sortDescending 
    match sizes with 
    | a :: b :: c :: _ -> a * b * c
    | _ -> failwith "?"
    
let solveB circuits connections = 
    match connectUntilOne connections circuits with 
    | Some ((x1, _, _), (x2, _, _)) -> x1 * x2
    | None -> failwith "?"
    
let run count fileName = 
    let lines = readLines fileName
    let boxes = lines |> List.map parse
    let connections = boxes |> getDistances |> List.map snd
    let circuits = boxes |> List.map singleton
    connections |> List.take count |> solveA |> printfn "%d"
    connections |> solveB circuits |> printfn "%d"

// run 10 "sample.txt"
run 1000 "input.txt"
