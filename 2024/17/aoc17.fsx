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
    outputs : int64 list 
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
      program = arr.[4] |> parseProgram
      outputs = [] }

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

let jump target computer = 
    { computer with pointer = target }

let output v computer = 
    { computer with outputs = v :: computer.outputs }

let division computer = 
    let operand = combo computer 
    printfn "operand %d" operand
    let numerator = computer.regA 
    let denominator = int64 (Math.Pow(2.0, double operand))
    let result = numerator / denominator
    printfn "division %d %d" numerator denominator
    result 

let xor v1 v2 : int64 = v1 ^^^ v2 

let modulo v : int64 = v % 8L

let adv (computer : Computer) : Computer = 
    printfn "adv"
    let result = division computer
    computer 
    |> writeA result  
    |> nextInstruction

let bdv computer = 
    printfn "adv"
    let result = division computer
    computer 
    |> writeB result  
    |> nextInstruction

let cdv computer = 
    printfn "cdv"
    let result = division computer
    computer 
    |> writeC result  
    |> nextInstruction

let bxl computer = 
    let operand = literal computer 
    let regB = computer.regB 
    let result = xor operand regB
    computer 
    |> writeB result 
    |> nextInstruction

let bst computer = 
    let operand = combo computer 
    let result = modulo operand
    computer
    |> writeB result
    |> nextInstruction

let jnz computer = 
    let regA = computer.regA 
    if regA = 0L then 
        computer |> nextInstruction
    else 
        let target = literal computer 
        computer |> jump target

let bxc computer =
    let _ = literal computer // legacy
    let regB = computer.regB 
    let regC = computer.regC 
    let result = xor regB regC
    computer 
    |> writeB result 
    |> nextInstruction

let out computer = 
    let operand = combo computer 
    let result = modulo operand 
    computer 
    |> output result 
    |> nextInstruction

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
