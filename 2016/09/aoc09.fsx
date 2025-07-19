// Advent of Code 2016. Day 09: Explosives in Cyberspace.
// dotnet fsi aoc09.fsx

open System
open System.IO

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let findLength (s : string) = 
    let parseMarker (chars : char list) = 
        let s = new string(chars |> List.toArray)
        match s.Split "x" with 
        | [|s1; s2|] -> int s1, int s2 
        | _ -> failwith <| sprintf "%s?" s
    let rec readUntilClose (acc : char list) (chars : char list) = 
        match chars with 
        | [] -> failwith "?"
        | ')' :: rest -> 
            acc |> List.rev, rest 
        | ch :: rest -> 
            readUntilClose (ch :: acc) rest
    let rec fn (len : int) (chars : char list) =
        match chars with 
        | [] -> len 
        | '(' :: rest -> 
            let before, after = readUntilClose [] rest
            let num, rep = parseMarker before
            let skipped = after |> List.skip num 
            fn (len + num * rep) skipped 
        | ch :: rest -> 
            printfn "%A" rest
            if ch < 'A' || ch > 'Z' then failwith <| sprintf "%c??" ch
            fn (len + 1) rest 
    s |> Seq.toList |> fn 0 

let run fileName = 
    let text = readText fileName
    "ADVENT" |> findLength |> printfn "%d"
    "A(1x5)BC" |> findLength |> printfn "%d"
    "(3x3)XYZ" |> findLength |> printfn "%d"
    "A(2x2)BCD(2x2)EFG" |> findLength |> printfn "%d"
    "(6x1)(1x3)A" |> findLength |> printfn "%d"
    "X(8x2)(3x3)ABCY" |> findLength |> printfn "%d"
    text |> findLength |> printfn "%d"

run "input.txt"
