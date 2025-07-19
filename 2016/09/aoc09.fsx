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

let rec findLengthV2 (s : string) : int64 = 
    // printfn "findLengthV2 %s (%d)" s s.Length
    let parseMarker (s : string) = 
        match s.Split "x" with 
        | [|s1; s2|] -> int s1, int64 s2 
        | _ -> failwith <| sprintf "%s?" s
    if s.Length = 0 then 0L 
    else 
        match s[0] with 
        | '(' -> 
            let ix = s.IndexOf(')')
            let markerStr = s.Substring(1, ix - 1)
            let afterStr = s.Substring(ix + 1)
            let charCount, repetitions = parseMarker markerStr
            let consumed = afterStr.Substring(0, charCount)
            let restStr = afterStr.Substring(charCount)
            let lengthConsumed = findLengthV2 consumed
            repetitions * lengthConsumed + findLengthV2 restStr
        | _ ->
            1L + findLengthV2 (s.Substring(1))

let run fileName = 
    let text = readText fileName
    // "ADVENT" |> findLength |> printfn "%d"
    // "A(1x5)BC" |> findLength |> printfn "%d"
    // "(3x3)XYZ" |> findLength |> printfn "%d"
    // "A(2x2)BCD(2x2)EFG" |> findLength |> printfn "%d"
    // "(6x1)(1x3)A" |> findLength |> printfn "%d"
    // "X(8x2)(3x3)ABCY" |> findLength |> printfn "%d"
    // text |> findLength |> printfn "%d"
    "(27x12)(20x12)(13x14)(7x10)(1x12)A" |> findLengthV2 |> printfn "%d"
    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" |> findLengthV2 |> printfn "%d"
    text |> findLengthV2 |> printfn "%d"

run "input.txt"
