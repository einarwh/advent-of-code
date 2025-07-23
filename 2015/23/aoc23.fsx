// Advent of Code 2015. Day 23: Opening the Turing Lock.
// dotnet fsi aoc23.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Register = A | B

type Offset = int 

type Instruction = 
    | Hlf of Register
    | Tpl of Register 
    | Inc of Register
    | Jmp of Offset 
    | Jie of Register * Offset 
    | Jio of Register * Offset 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let parseRegister (s : string) = 
    if s = "a" then A else B

let tryParseHlf (s : String) : Instruction option = 
    let m = Regex.Match(s, "^hlf (a|b)$")
    if m.Success then
        let r = parseRegister m.Groups.[1].Value
        Some (Hlf r)
    else
        None

let tryParseTpl (s : String) : Instruction option = 
    let m = Regex.Match(s, "^tpl (a|b)$")
    if m.Success then
        let r = parseRegister m.Groups.[1].Value
        Some (Tpl r)
    else
        None

let tryParseInc (s : String) : Instruction option = 
    let m = Regex.Match(s, "^inc (a|b)$")
    if m.Success then
        let r = parseRegister m.Groups.[1].Value
        Some (Inc r)
    else
        None

let tryParseJmp (s : String) : Instruction option = 
    let m = Regex.Match(s, "^jmp (\S+)$")
    if m.Success then
        let o = int <| m.Groups.[1].Value
        Some (Jmp o)
    else
        None

let tryParseJie (s : String) : Instruction option = 
    let m = Regex.Match(s, "^jie (a|b), (\S+)$")
    if m.Success then
        let r = parseRegister m.Groups.[1].Value
        let o = int <| m.Groups.[2].Value
        Some (Jie (r, o))
    else
        None

let tryParseJio (s : String) : Instruction option = 
    let m = Regex.Match(s, "^jio (a|b), (\S+)$")
    if m.Success then
        let r = parseRegister m.Groups.[1].Value
        let o = int <| m.Groups.[2].Value
        Some (Jio (r, o))
    else
        None

let tryParse (s : String) : Instruction option = 
   s 
   |> tryParseHlf 
   |> Option.orElseWith (fun () -> tryParseTpl s)
   |> Option.orElseWith (fun () -> tryParseInc s)
   |> Option.orElseWith (fun () -> tryParseJmp s)
   |> Option.orElseWith (fun () -> tryParseJie s)
   |> Option.orElseWith (fun () -> tryParseJio s)

let hlf r (ptr, (a, b)) = 
    (ptr + 1, match r with | A -> (a / 2, b) | B -> (a, b / 2))

let tpl r (ptr, (a, b)) = 
    (ptr + 1, match r with | A -> (a * 3, b) | B -> (a, b * 3))

let inc r (ptr, (a, b)) = 
    (ptr + 1, match r with | A -> (a + 1, b) | B -> (a, b + 1))

let jmp o (ptr, (a, b)) = 
    (ptr + o, (a, b))

let jie r o (ptr, (a, b)) = 
    let v = match r with | A -> a | B -> b
    let tgt = if v % 2 = 0 then (ptr + o) else ptr + 1
    (tgt, (a, b))

let jio r o (ptr, (a, b)) = 
    let v = match r with | A -> a | B -> b
    let tgt = if v = 1 then (ptr + o) else ptr + 1
    (tgt, (a, b))

let runProgram (a, b) (instructions : Instruction array) = 
    let rec loop (ptr : int, (a : int, b : int)) = 
        if ptr < 0 || ptr >= instructions.Length then 
            b
        else 
            match instructions[ptr] with 
            | Hlf r -> hlf r (ptr, (a, b)) |> loop
            | Tpl r -> tpl r (ptr, (a, b)) |> loop
            | Inc r -> inc r (ptr, (a, b)) |> loop
            | Jmp o -> jmp o (ptr, (a, b)) |> loop
            | Jie (r, o) -> jie r o (ptr, (a, b)) |> loop
            | Jio (r, o) -> jio r o (ptr, (a, b)) |> loop
    loop (0, (a, b))

let run fileName = 
    let lines = readLines fileName
    let instructions = lines |> Array.choose tryParse
    instructions |> runProgram (0, 0) |> printfn "%A"
    instructions |> runProgram (1, 0) |> printfn "%A"

run "input.txt"
