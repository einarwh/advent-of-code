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

let rec rollNorth (lines : string list) : string list = 
    match lines with 
    | a :: b :: rest -> 
        let (a', b') = rollNorthStep (a, b)
        a' :: rollNorth (b' :: rest)
    | _ -> lines 

let rollSouthStep (s1, s2) = 
    let line1 = s1 |> Seq.toList
    let line2 = s2 |> Seq.toList
    let zipped = List.zip line1 line2 
    let rolled = zipped |> List.map (fun (a, b) -> if a = 'O' && b = '.' then (b, a) else (a, b))
    let s1' = rolled |> List.map fst |> List.toArray |> String
    let s2' = rolled |> List.map snd |> List.toArray |> String
    (s1', s2')

let rec rollSouth (lines : string list) : string list = 
    let folder upper acc = 
        match acc with 
        | [] -> [ upper ]
        | lower :: rest -> 
            let (upper', lower') = rollSouthStep (upper, lower)
            upper' :: lower' :: rest
    List.foldBack folder lines []

let rec rollEastStep (chars : char list) : char list = 
    match chars with 
    | 'O' :: '.' :: rest -> '.' :: rollEastStep ('O' :: rest)
    | ch :: rest -> ch :: rollEastStep rest 
    | [] -> []

let rollLineEast (s : string) : string = 
    let cs = s |> Seq.toList 
    rollEastStep cs |> List.toArray |> String

let rollEast (lines : string list) : string list = 
    lines |> List.map rollLineEast

let rec rollWestStep (chars : char list) : char list = 
    let folder (left : char) (acc : char list) : char list = 
        match acc with 
        | [] -> [ left ]
        | right :: rest -> 
            match (left, right) with 
            | ('.', 'O') -> 'O' :: '.' :: rest 
            | _ -> left :: acc 
    List.foldBack folder chars []

let rollLineWest (s : string) : string = 
    let cs = s |> Seq.toList 
    rollWestStep cs |> List.toArray |> String

let rollWest (lines : string list) : string list = 
    lines |> List.map rollLineWest

let rec tilt roll current = 
    let tilted = roll current 
    if tilted = current then current else tilt roll tilted 

let tiltNorth = tilt rollNorth 

let tiltSouth = tilt rollSouth

let tiltEast = tilt rollEast

let tiltWest = tilt rollWest 

let cycle = tiltNorth >> tiltWest >> tiltSouth >> tiltEast

let solve limit lines = 
    let rec loop n seen current = 
        if n >= limit then 
            (n, -1, seen) 
        else 
            if List.contains current seen then 
                let ix = List.findIndex ((=) current) seen 
                (n, ix, current :: seen) 
            else 
                let seen' = current :: seen
                let next = cycle current 
                loop (n + 1) seen' next 
    loop 0 [] lines 

let countRocks (line : string) : int = 
    line |> Seq.filter ((=) 'O') |> Seq.length 

let calculateLoad (lines : string list) = 
    let len = lines |> List.length 
    lines |> List.mapi (fun i line -> (len - i) * countRocks line) |> List.sum
    
let printPattern (i : int) (pattern : string list) = 
    printfn "%d" i
    pattern |> List.iter (printfn "%A")

let run fileName =
    let lines = readLines fileName |> Array.toList

    printfn "tilted north"
    let tiltedNorth = lines |> tiltNorth
    // tiltedNorth |> calculateLoad |> printfn "%d"
    // tiltedNorth |> List.iter (printfn "%A")

    printfn "tilted south"
    let tiltedSouth = lines |> tiltSouth
    // tiltedSouth |> List.iter (printfn "%A")
    // tiltedSouth |> calculateLoad |> printfn "%d"

    printfn "tilted east"
    let tiltedEast = lines |> tiltEast
    // tiltedEast |> List.iter (printfn "%A")
    // tiltedEast |> calculateLoad |> printfn "%d"
    
    printfn "tilted west"
    let tiltedWest = lines |> tiltWest
    // tiltedWest |> List.iter (printfn "%A")
    // tiltedWest |> calculateLoad |> printfn "%d"

    printfn "cycle 1"
    let cycle1 = lines |> cycle 
    // cycle1 |> List.iter (printfn "%A")

    let foo = solve 1000 lines 
    foo |> printfn "%A"
    let (lastIndex, firstIndex, seen) = foo 
    printfn "last index: %d" lastIndex 
    printfn "first index: %d" firstIndex 
    seen |> List.iteri (printPattern)


"sample" |> run
