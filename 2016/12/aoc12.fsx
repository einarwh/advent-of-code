// Advent of Code 2016. Day 12: Leonardo's Monorail.
// dotnet fsi aoc12.fsx

open System
open System.IO
open System.Text.RegularExpressions

type R = A | B | C | D

type Source = 
    | Register of R 
    | Value of int

type Instruction = 
    | Cpy of Source * R
    | Inc of R
    | Dec of R
    | Jnz of Source * int 

let parseRegister (s : string) = 
    match s with 
    | "a" -> A
    | "b" -> B
    | "c" -> C
    | _ -> D

let parseSource (str : string) = 
    match Int32.TryParse str with
    | true, n -> Value n
    | _ -> Register (parseRegister str) 

let tryParseCpy (s : String) : Instruction option = 
    let m = Regex.Match(s, "^cpy ([a-d]|\d+) ([a-d])$")
    if m.Success then
        let source = parseSource m.Groups.[1].Value
        let r = parseRegister m.Groups.[2].Value
        Some (Cpy (source, r))
    else
        None

let tryParseInc (s : String) : Instruction option = 
    let m = Regex.Match(s, "^inc ([a-d])$")
    if m.Success then
        let r = parseRegister m.Groups.[1].Value
        Some (Inc r)
    else
        None

let tryParseDec (s : String) : Instruction option = 
    let m = Regex.Match(s, "^dec ([a-d])$")
    if m.Success then
        let r = parseRegister m.Groups.[1].Value
        Some (Dec r)
    else
        None

let tryParseJnz (s : String) : Instruction option = 
    let m = Regex.Match(s, "^jnz ([a-d]|\d+) (-?\d+)$")
    if m.Success then
        let source = parseSource m.Groups.[1].Value
        let offset = int <| m.Groups.[2].Value
        Some (Jnz (source, offset))
    else
        None

let tryParse (s : String) : Instruction option =
   s
   |> tryParseCpy
   |> Option.orElseWith (fun () -> tryParseInc s)
   |> Option.orElseWith (fun () -> tryParseDec s)
   |> Option.orElseWith (fun () -> tryParseJnz s)
   |> Option.orElseWith (fun () -> failwith <| sprintf "Failed to parse '%s'!" s)

let getRegisterValue (a, b, c, d) r = 
    match r with 
    | A -> a 
    | B -> b 
    | C -> c 
    | D -> d 

let setRegisterValue (a, b, c, d) r v = 
    match r with 
    | A -> (v, b, c, d)
    | B -> (a, v, c, d)
    | C -> (a, b, v, d)
    | D -> (a, b, c, v)

let cpy ptr registers src reg = 
    let value = 
        match src with 
        | Register r -> getRegisterValue registers r
        | Value n -> n
    (ptr + 1, setRegisterValue registers reg value)

let inc ptr registers reg = 
    let value = getRegisterValue registers reg
    (ptr + 1, setRegisterValue registers reg (value + 1))

let dec ptr registers reg = 
    let value = getRegisterValue registers reg
    (ptr + 1, setRegisterValue registers reg (value - 1))

let jnz ptr registers src off = 
    let value = 
        match src with 
        | Register r -> getRegisterValue registers r
        | Value n -> n
    let ptr' = if value = 0 then ptr + 1 else ptr + off
    (ptr', registers)

let runProgram initial (instructions : Instruction array) = 
    let rec loop (ptr, registers) = 
        if ptr < 0 || ptr >= instructions.Length then 
            let (a, b, c, d) = registers
            a
        else 
            match instructions[ptr] with 
            | Cpy (src, reg) -> 
                cpy ptr registers src reg |> loop
            | Inc reg -> 
                inc ptr registers reg |> loop
            | Dec reg -> 
                dec ptr registers reg |> loop
            | Jnz (src, off) -> 
                jnz ptr registers src off |> loop
    loop (0, initial)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName
    let instructions = lines |> Array.choose tryParse
    instructions |> runProgram (0, 0, 0, 0) |> printfn "%d"
    instructions |> runProgram (0, 0, 1, 0) |> printfn "%d"

run "input.txt"
