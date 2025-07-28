// Advent of Code 2022. 
// Day 10: Cathode-Ray Tube.
// dotnet fsi aoc10.fsx

open System.IO
open System.Text.RegularExpressions

module List =
    let safeSkip n lst =
        lst
        |> List.mapi (fun ix it -> ix, it)
        |> List.choose (fun (ix, it) -> if ix >= n then Some(it) else None)

type Instruction = 
    | NoOp 
    | AddX of int

let tryParseNoOp (s : string) : Instruction option = 
    let pattern = "^noop$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        Some NoOp
    else 
        None 

let tryParseAddX (s : string) : Instruction option = 
    let pattern = "^addx (.+)$"
    let m = Regex.Match(s, pattern)
    if m.Success then
        let v = m.Groups.[1].Value |> int
        Some <| AddX v
    else 
        None 

let tryParseInstruction (s : string) : Instruction option =
    s |> tryParseNoOp 
    |> Option.orElse (s |> tryParseAddX)

let instructionToValues (instruction : Instruction) : int list = 
    match instruction with 
    | NoOp -> [0]
    | AddX v -> [0; v]

let sampleSignals xs = 
    let rec fn (signalNo: int) (signals : int list) (xs : int list) = 
        match xs with 
        | [] -> signals |> List.rev 
        | h :: _ -> 
            fn (signalNo + 40) (h * signalNo :: signals) (xs |> List.safeSkip 40)
    fn 20 [] (xs |> List.safeSkip 19)

let overlap c pos = 
    c = pos - 1 || c = pos || c = pos + 1

let toPixel cycle pos = 
    let c = cycle % 40 
    if overlap c pos then "#" else "."

let part1 xs = 
    xs |> sampleSignals |> List.sum |> printfn "%A"

let part2 xs = 
    xs 
    |> List.take 240 
    |> List.mapi toPixel
    |> List.chunkBySize 40
    |> List.map (String.concat "")
    |> String.concat "\n"
    |> printfn "%s"

let run instructions = 
    let folder (x, vals) v = 
        let x' = x + v 
        (x', x' :: vals)
    let xsTail = 
        instructions 
        |> List.collect instructionToValues
        |> List.fold folder (1, []) |> snd |> List.rev 
    let xs = 1 :: xsTail 
    xs |> part1
    xs |> part2

"input.txt"
|> File.ReadAllLines
|> Array.toList
|> List.choose tryParseInstruction
|> run 
