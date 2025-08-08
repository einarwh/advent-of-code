// Advent of Code 2020. Day 8: Handheld Halting.
// dotnet fsi aoc08.fsx

open System.IO
open System.Text.RegularExpressions

type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

type Termination =
    | Loop of int
    | Normal of int

let parse (s : string) : Instruction =
    let m = Regex.Match(s, "^([a-z]{3}) (\+|\-)(\d+)")
    let opStr = m.Groups.[1].Value
    let signStr = m.Groups.[2].Value
    let argStr = m.Groups.[3].Value
    let unsignedArg = int argStr
    let arg = if signStr = "+" then unsignedArg else - unsignedArg
    if opStr = "acc" then
        Acc arg
    elif opStr = "jmp" then
        Jmp arg
    else
        Nop arg

let execute (instruction : Instruction) (pos: int, acc : int) =
    match instruction with
    | Nop _ -> (pos + 1, acc)
    | Acc n -> (pos + 1, acc + n)
    | Jmp n -> (pos + n, acc)

let run (instructions : Instruction array) : Termination =
    let rec r (history : int list) (pos : int, acc : int) : Termination =
        if List.contains pos history then
            Loop acc
        else
            let instruction = instructions.[pos]
            let next = execute instruction (pos, acc)
            match next with
            | (pos', acc') when pos' = Array.length instructions ->
                Normal acc'
            | _ ->
                r (pos :: history) next
    r [] (0, 0)

let possibleCorruptions (instructions : Instruction array) =
    instructions
    |> Array.mapi (fun i x -> match x with | Acc _ -> None | _ -> Some i)
    |> Array.choose id

let swap instruction =
    match instruction with
    | Nop x -> Jmp x
    | Jmp x -> Nop x
    | Acc x -> Acc x 

let swapAt (instructions : Instruction array) (ix : int) =
    let copy = Array.copy instructions
    copy.[ix] <- swap instructions.[ix]
    copy

let permute (instructions : Instruction array) =
    instructions
    |> possibleCorruptions
    |> Array.map (swapAt instructions)
    |> Array.toList

let runOnce (instructions : Instruction array) : int =
    match run instructions with
    | Loop n -> n
    | Normal n -> n 
    
let runUntilFixed (instructions : Instruction array) : int =
    let rec loop (programs : Instruction array list) : int =
        match programs with
        | [] -> 0
        | h :: t ->
            match run h with
            | Loop _ -> loop t
            | Normal n -> n
    loop (instructions :: permute instructions)

let solve lines =
    let instructions = lines |> Array.map parse
    instructions |> runOnce |> printfn "%d" 
    instructions |> runUntilFixed |> printfn "%d" 

"input.txt" |> File.ReadAllLines |> solve 