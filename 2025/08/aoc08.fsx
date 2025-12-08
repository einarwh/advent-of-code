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
    let sq n = n * n |> double
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

let useConnection (box1, box2) circuits = 
    // printfn "\nuseConnection %A" (box1, box2)
    let a = circuits |> List.filter (Set.contains box1)
    // printfn "includes box1 %A" a
    let b = circuits |> List.filter (Set.contains box2)
    // printfn "includes box2 %A" b
    let included = a @ b 
    // printfn "included %A" included
    let excluded = circuits |> List.except included
    // printfn "excluded %A" excluded 
    let connected = included |> List.reduce (fun a b -> Set.union a b) 
    // printfn "connected circuit %A" connected
    let result = connected :: excluded 
    // printfn "result: %A" result
    result

let connect connections circuits = 
    let rec loop connections circuits =
        match connections with 
        | [] -> circuits  
        | conn :: rest -> 
            let circuits' = useConnection conn circuits 
            loop rest circuits' 
    loop connections circuits 
    
let connectUntilOne connections circuits = 
    let rec loop connections circuits =
        match connections with 
        | [] -> None   
        | conn :: rest -> 
            let circuits' = useConnection conn circuits 
            if List.length circuits' = 1 then 
                Some conn 
            else 
                loop rest circuits' 
    loop connections circuits 
    
let solve count boxes = 
    printfn "solve %d" count
    let distances = boxes |> getDistances
    printfn "# distances %d" (List.length distances)
    let shortest = distances |> List.take 100
    shortest |> List.iter (printfn "%A")
    let connections = distances |> List.take count |> List.map snd
    let circuits = connections |> List.collect (fun (a, b) -> [a; b]) |> List.distinct |> List.map (fun a -> Set.empty |> Set.add a) 
    let result = connect connections circuits
    let sizes = result |> List.map Set.count |> List.sortDescending 
    printfn "%A" sizes
    match sizes with 
    | a :: b :: c :: _ -> printfn "%d" (a * b * c)
    | _ -> failwith "?"
    let allConnections = distances |> List.map snd
    let allCircuits = boxes |> List.map (fun b -> Set.empty |> Set.add b)
    let res2 = connectUntilOne allConnections allCircuits
    match res2 with 
    | Some ((x1, _, _), (x2, _, _)) -> printfn "%d" (x1 * x2)
    | None -> failwith "?"
    printfn "%A" res2
    0


let run conns fileName = 
    let lines = readLines fileName
    let boxes = lines |> List.map parse
    solve conns boxes |> printfn "%d"

run 10 "sample.txt"
run 1000 "input.txt"
