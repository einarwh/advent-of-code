// Advent of Code 2024. Day 17
// dotnet fsi aoc17.fsx

open System
open System.IO

type Computer = {
    regA : int64
    regB : int64
    regC : int64
    pointer : int64
    program : int64 array 
}

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parseRegister (s : string) : int64 = 
    s |> split ": " |> Array.item 1 |> int64

let parseProgram (s : string) : int64 array = 
    s |> split ": " |> Array.item 1 |> split "," |> Array.map int64

let parseComputer (arr : string array) = 
    { regA = arr.[0] |> parseRegister 
      regB = arr.[1] |> parseRegister 
      regC = arr.[2] |> parseRegister 
      pointer = 0L
      program = arr.[4] |> parseProgram }

let readOpcode computer : int = 
    let pt = computer.pointer
    int (computer.program.[int pt])

let literal computer = 
    let pt = computer.pointer + 1L
    int (computer.program.[int pt])

let combo computer = 
    let n = literal computer 
    match n with 
    | 0 | 1 | 2 | 3 -> int64 n
    | 4 -> computer.regA 
    | 5 -> computer.regB 
    | 6 -> computer.regC 
    | 7 -> failwith "reserved"
    | _ -> failwith "?"

let writeA value computer = 
    { computer with regA = value }

let writeB value computer = 
    { computer with regB = value }

let writeC value computer = 
    { computer with regC = value }

let nextInstruction computer = 
    { computer with pointer = computer.pointer + 2L }

let adv (computer : Computer) : Computer = 
    let operand = combo computer 
    printfn "adv %d" operand
    let numerator = computer.regA 
    let denominator = int64 (Math.Pow(2.0, double operand))
    let result = numerator / denominator
    printfn "division %d %d" numerator denominator
    computer 
    |> writeA result 
    |> nextInstruction

let bdv computer = 
    let operand = combo computer 
    computer 

let cdv computer = 
    let operand = combo computer 
    computer 

let bxl computer = 
    let operand = literal computer 
    computer 

let bst computer = 
    let operand = combo computer 
    computer 

let jnz computer = 
    computer 

let bxc computer =
    computer 

let out computer = 
    let operand = combo computer 
    computer 

let execute (computer : Computer) = 
    let computer' : Computer = 
        let opcode = readOpcode computer 
        printfn "execute, read opcode %d at %d" opcode computer.pointer
        match opcode with 
        | 0 -> adv computer 
        | 1 -> bxl computer 
        | 2 -> bst computer 
        | 3 -> jnz computer 
        | 4 -> bxc computer 
        | 5 -> out computer 
        | 6 -> bdv computer 
        | 7 -> cdv computer 
        | _ -> failwith "?"
    0

let run fileName = 
    let text = File.ReadAllText fileName |> trim |> split "\n"
    let computer = parseComputer text 
    computer |> printfn "%A"
    execute computer
    0

run "sample"
