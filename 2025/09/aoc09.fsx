// Advent of Code 2025. Day 09: Movie Theater.
// dotnet fsi aoc09.fsx

open System
open System.IO

let readStrings = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let parse (s : string) = 
    match s.Split "," |> Array.map int64 with 
    | [|a; b|] -> (a, b)
    | _ -> failwith "?"

let area (x1, y1) (x2, y2) = 
    (1L + abs (x2 - x1)) * (1L + abs (y2 - y1))

let getRectangles tiles = 
    let rec loop acc tiles = 
        match tiles with 
        | a :: rest -> 
            let rs = rest |> List.map (fun b -> area a b, (a, b)) 
            loop (rs :: acc) rest 
        | _ -> 
            acc |> List.concat |> List.sortByDescending fst 
    loop [] tiles

let connect tiles = 
    match tiles with 
    | [] -> failwith "?"
    | first :: _ -> 
        let rec loop acc tiles = 
            match tiles with 
            | [] -> failwith "?"
            | [ last ] -> (last, first) :: acc |> List.rev
            | a :: b :: t -> 
                loop ((a, b) :: acc) (b :: t)
        loop [] tiles 

let toLine ((x1, y1), (x2, y2)) = 
    if x1 = x2 then 
        let (yStart, yEnd) = if y1 < y2 then (y1, y2) else (y2, y1)
        (x1, yStart), (x1, yEnd)
    else 
        let (xStart, xEnd) = if x1 < x2 then (x1, x2) else (x2, x1)
        (xStart, y1), (xEnd, y1)

let contained verticals horizontals ((x1, y1), (x2, y2)) =
    let vs = [ toLine ((x1, y1), (x1, y2)); toLine ((x2, y1), (x2, y2)) ]
    let hs = [ toLine ((x1, y1), (x2, y1)); toLine ((x1, y2), (x2, y2)) ]
    let crossed = v
    true 

let run fileName = 
    let reds = fileName |> readStrings |> List.map parse 
    let rectangles = getRectangles reds 
    rectangles |> List.head |> fst |> printfn "%d"
    let pairs = reds |> connect 
    pairs |> List.iter (printfn "%A")
    let lines = pairs |> List.map toLine 
    let verticals = lines |> List.filter (fun ((x1, _), (x2, _)) -> x1 = x2)
    let horizontals = lines |> List.except verticals
    printfn "%A" verticals 
    printfn "%A" horizontals 
    rectangles |> List.toSeq |> Seq.filter (snd >> contained verticals horizontals) |> Seq.head |> fst |> printfn "%d"
    // let sets = pairs |> List.map (expand >> Set.ofList)
    // let boundary = sets |> List.reduce Set.union
    0

run "sample.txt"
