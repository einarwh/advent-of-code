// Advent of Code 2024. Day 17: Chronospatial Computer.
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

let parseRegister s =
    s |> split ": " |> Array.item 1 |> int64

let parseProgram s =
    s |> split ": " |> Array.item 1 |> split "," |> Array.map int64

let parseComputer (arr : string array) =
    { regA = arr.[0] |> parseRegister
      regB = arr.[1] |> parseRegister
      regC = arr.[2] |> parseRegister
      pointer = 0L
      program = arr.[4] |> parseProgram
      outputs = [] }

let tryReadOpcode computer =
    let pt = computer.pointer
    if pt < Array.length computer.program then
        computer.program.[int pt] |> int |> Some
    else
        None

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

let rec pow x y = if y < 1L then 1L else x * pow x (y - 1L)

let division computer =
    let numerator = computer.regA
    let denominator = pow 2 (combo computer)
    numerator / denominator

let xor v1 v2 : int64 = v1 ^^^ v2

let modulo v : int64 = v % 8L

let adv (computer : Computer) : Computer =
    computer
    |> writeA (division computer)
    |> nextInstruction

let bdv computer =
    computer
    |> writeB (division computer)
    |> nextInstruction

let cdv computer =
    computer
    |> writeC (division computer)
    |> nextInstruction

let bxl computer =
    computer
    |> writeB (xor (literal computer) computer.regB)
    |> nextInstruction

let bst computer =
    computer
    |> writeB (modulo (combo computer))
    |> nextInstruction

let jnz computer =
    if computer.regA = 0L then
        computer |> nextInstruction
    else
        computer |> jump (literal computer)

let bxc computer =
    let _ = literal computer // legacy
    computer
    |> writeB (xor computer.regB computer.regC)
    |> nextInstruction

let out computer =
    computer
    |> output (modulo (combo computer))
    |> nextInstruction

let rec execute computer =
    match tryReadOpcode computer with
    | None ->
        computer.outputs |> List.rev |> List.toArray 
    | Some opcode ->
        let c =
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
        c |> execute

let printProgram program = 
    program |> Array.map string |> String.concat "," |> printfn "%s"

// 35184372088832
let quine computer = 
    let len = computer.program |> Array.length
    let checkTarget opIndex target candidateA = 
        let p = execute { computer with regA = candidateA }
        p.[opIndex] = target 
    let rec loop ix a = 
        let opIndex = len - ix 
        if ix > len then 
            Some a
        else 
            let target = computer.program[opIndex]
            let offset = pow 8L ((int64 len) - (int64 ix))
            let candidates = 
                [ 0L .. 7L ] 
                |> List.map (fun j -> a + j * offset)
                |> List.choose (fun ca -> if checkTarget opIndex target ca then Some ca else None)
            candidates |> List.tryPick (loop (ix + 1))
    let a0 = pow 8L ((int64 len) - 1L)
    match loop 1 a0 with 
    | Some a -> a 
    | None -> failwith ":("

let run fileName =
    let text = File.ReadAllText fileName |> trim |> split "\n"
    let computer = parseComputer text
    computer |> execute |> printProgram 
    computer |> quine |> printfn "%d"

run "input"
