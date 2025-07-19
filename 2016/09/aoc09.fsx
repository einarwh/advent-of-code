// Advent of Code 2016. Day 09: Explosives in Cyberspace.
// dotnet fsi aoc09.fsx

open System
open System.IO

let readText fileName = 
    File.ReadAllText(fileName).Trim()

// let findLength (s : string) = 
//     let parseMarker (chars : char list) = 
//         let s = new string(chars |> List.toArray)
//         match s.Split "x" with 
//         | [|s1; s2|] -> int s1, int s2 
//         | _ -> failwith <| sprintf "%s?" s
//     let rec readUntilClose (acc : char list) (chars : char list) = 
//         match chars with 
//         | [] -> failwith "?"
//         | ')' :: rest -> 
//             acc |> List.rev, rest 
//         | ch :: rest -> 
//             readUntilClose (ch :: acc) rest
//     let rec fn (len : int) (chars : char list) =
//         match chars with 
//         | [] -> len 
//         | '(' :: rest -> 
//             let before, after = readUntilClose [] rest
//             let num, rep = parseMarker before
//             let skipped = after |> List.skip num 
//             fn (len + num * rep) skipped 
//         | ch :: rest -> 
//             if ch < 'A' || ch > 'Z' then failwith <| sprintf "%c??" ch
//             fn (len + 1) rest 
//     s |> Seq.toList |> fn 0 

let rec findLen (isV2 : bool) (s : string) : int64 = 
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
            let lengthConsumed = 
                if isV2 then findLen isV2 consumed else consumed.Length
            repetitions * lengthConsumed + findLen isV2 restStr
        | _ ->
            1L + findLen isV2 (s.Substring(1))

let findLengthV1 = findLen false 
let findLengthV2 = findLen true 

let verify finder expectedResult inputStr = 
    let actualResult = finder inputStr 
    if expectedResult = actualResult then 
        printfn "OK - decompressed length of %s verified as %d" inputStr expectedResult
    else 
        failwith <| sprintf "Expected decompressed length of %s to be %d but was %d" inputStr expectedResult actualResult

let verify2 finder (expectedResult : int64) inputStr = 
    let actualResult = finder inputStr 
    if expectedResult = actualResult then 
        printfn "Decompressed length of %s verified as %d" inputStr expectedResult
    else 
        failwith <| sprintf "Expected decompressed length of %s to be %d but was %d" inputStr expectedResult actualResult

let verifyPart1() = 
    "ADVENT" |> verify findLengthV1 6
    "A(1x5)BC" |> verify findLengthV1 7
    "(3x3)XYZ" |> verify findLengthV1 9
    "A(2x2)BCD(2x2)EFG" |> verify findLengthV1 11
    "(6x1)(1x3)A" |> verify findLengthV1 6
    "X(8x2)(3x3)ABCY" |> verify findLengthV1 18

let verifyPart2() = 
    "(3x3)XYZ" |> verify2 findLengthV2 9L
    "X(8x2)(3x3)ABCY" |> verify2 findLengthV2 20L
    "(27x12)(20x12)(13x14)(7x10)(1x12)A" |> verify2 findLengthV2 241920L
    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" |> verify2 findLengthV2 445L

let run fileName = 
    let text = readText fileName
    verifyPart1()
    verifyPart2()
    text |> findLen false |> printfn "%d"
    text |> findLen true |> printfn "%d"

run "input.txt"
