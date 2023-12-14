// Advent of Code 2023. Day 14: Parabolic Reflector Dish
// dotnet fsi aoc14.fsx

open System
open System.IO

let readLines =
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let rollNorthStep (s1, s2) = 
    let line1 = s1 |> Seq.toList
    let line2 = s2 |> Seq.toList
    let zipped = List.zip line1 line2 
    let rolled = zipped |> List.map (fun (a, b) -> if a = '.' && b = 'O' then (b, a) else (a, b))
    let s1' = rolled |> List.map fst |> List.toArray |> String
    let s2' = rolled |> List.map snd |> List.toArray |> String
    (s1', s2')

let rec north lines = 
    match lines with 
    | a :: b :: rest -> 
        let (a', b') = rollNorthStep (a, b)
        a' :: north (b' :: rest)
    | _ -> lines 

let rollSouthStep (s1, s2) = 
    let line1 = s1 |> Seq.toList
    let line2 = s2 |> Seq.toList
    let zipped = List.zip line1 line2 
    let rolled = zipped |> List.map (fun (a, b) -> if a = 'O' && b = '.' then (b, a) else (a, b))
    let s1' = rolled |> List.map fst |> List.toArray |> String
    let s2' = rolled |> List.map snd |> List.toArray |> String
    (s1', s2')

let rec south lines = 
    let folder upper acc = 
        match acc with 
        | [] -> [ upper ]
        | lower :: rest -> 
            let (upper', lower') = rollSouthStep (upper, lower)
            upper' :: lower' :: rest
    List.foldBack folder lines []

let rec rollEastStep chars = 
    match chars with 
    | 'O' :: '.' :: rest -> '.' :: rollEastStep ('O' :: rest)
    | ch :: rest -> ch :: rollEastStep rest 
    | [] -> []

let rollLineEast = Seq.toList >> rollEastStep >> List.toArray >> String

let east = List.map rollLineEast

let rec rollWestStep chars = 
    let folder left acc = 
        match acc with 
        | [] -> [ left ]
        | right :: rest -> 
            match (left, right) with 
            | ('.', 'O') -> 'O' :: '.' :: rest 
            | _ -> left :: acc 
    List.foldBack folder chars []

let rollLineWest = Seq.toList >> rollWestStep >> List.toArray >> String

let west = List.map rollLineWest

let rec tilt roll current = 
    let tilted = roll current 
    if tilted = current then current else tilt roll tilted 

let cycle = tilt north >> tilt west >> tilt south >> tilt east

let calculateLoad lines = 
    let countRocks = Seq.filter ((=) 'O') >> Seq.length 
    let len = lines |> List.length 
    lines |> List.mapi (fun i line -> (len - i) * countRocks line) |> List.sum
    
let solve limit lines = 
    let rec loop n seen current = 
        if n > limit then 
            None
        else 
            if List.contains current seen then 
                let interval = 1 + List.findIndex ((=) current) seen 
                let preceding = List.length seen - interval
                let sequence = seen |> List.rev |> List.skip preceding
                let ix = (limit - preceding) % (interval)
                let chosen = sequence |> List.item ix 
                chosen |> List.iter (printfn "%A")
                let load = chosen |> calculateLoad
                Some load
            else 
                loop (n + 1) (current :: seen) (cycle current) 
    loop 0 [] lines 

let run fileName =
    let lines = readLines fileName |> Array.toList
    let tiltedNorth = lines |> tilt north
    tiltedNorth |> calculateLoad |> printfn "%d"
    let limit = 1000000000
    match solve limit lines with 
    | None -> printfn "?"
    | Some load -> printfn "%d" load 

"sample" |> run
