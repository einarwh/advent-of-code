// Advent of Code 2017. Day 18: Duet.
// dotnet fsi aoc18.fsx

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type RegName = string

type Value = 
    | Reg of RegName
    | Num of int64

type Instruction = 
    | Snd of RegName 
    | Set of RegName * Value 
    | Add of RegName * Value 
    | Mul of RegName * Value 
    | Mod of RegName * Value 
    | Rcv of RegName 
    | Jgz of Value * Value 

type State = Terminated | Waiting | Running 

type Program = {
    ptr : int 
    instructions : Instruction array 
    state : State
    duet : bool
    sound : int64
    registers : Map<RegName, int64>
    inbox : Queue<int64> 
    outbox : Queue<int64>
}

let parseValue (s : string) : Value = 
    match Int32.TryParse s with
    | true, n -> Num n
    | _ -> Reg s

let tryParseSnd (line : string) : Instruction option =
    let m = Regex.Match(line, "^snd ([a-z]|\d+)$")
    if m.Success then
        let s = m.Groups.[1].Value
        let inst = Snd s
        Some inst
    else
        None

let tryParseSet (line : string) : Instruction option =
    let m = Regex.Match(line, "^set ([a-z]) ([a-z]|\d+)$")
    if m.Success then
        let r = m.Groups.[1].Value
        let s = m.Groups.[2].Value
        let inst = Set (r, parseValue s)
        Some inst
    else
        None

let tryParseAdd (line : string) : Instruction option =
    let m = Regex.Match(line, "^add ([a-z]) ([a-z]|-?\d+)$")
    if m.Success then
        let r = m.Groups.[1].Value
        let s = m.Groups.[2].Value
        let inst = Add (r, parseValue s)
        Some inst
    else
        None

let tryParseMul (line : string) : Instruction option =
    let m = Regex.Match(line, "^mul ([a-z]) ([a-z]|-?\d+)$")
    if m.Success then
        let r = m.Groups.[1].Value
        let s = m.Groups.[2].Value
        let inst = Mul (r, parseValue s)
        Some inst
    else
        None

let tryParseMod (line : string) : Instruction option =
    let m = Regex.Match(line, "^mod ([a-z]) ([a-z]|-?\d+)$")
    if m.Success then
        let r = m.Groups.[1].Value
        let s = m.Groups.[2].Value
        let inst = Mod (r, parseValue s)
        Some inst
    else
        None

let tryParseRcv (line : string) : Instruction option =
    let m = Regex.Match(line, "^rcv ([a-z]|-?\d+)$")
    if m.Success then
        let s = m.Groups.[1].Value
        let inst = Rcv s
        Some inst
    else
        None

let tryParseJgz (line : string) : Instruction option =
    let m = Regex.Match(line, "^jgz ([a-z]|-?\d+) ([a-z]|-?\d+)$")
    if m.Success then
        let s1 = m.Groups.[1].Value
        let s2 = m.Groups.[2].Value
        let v1 = parseValue s1 
        let v2 = parseValue s2 
        let inst = Jgz (v1, v2)
        Some inst
    else
        None

let tryParse (s : String) : Instruction option =
   s
   |> tryParseSnd
   |> Option.orElseWith (fun () -> tryParseSet s)
   |> Option.orElseWith (fun () -> tryParseAdd s)
   |> Option.orElseWith (fun () -> tryParseMul s)
   |> Option.orElseWith (fun () -> tryParseMod s)
   |> Option.orElseWith (fun () -> tryParseRcv s)
   |> Option.orElseWith (fun () -> tryParseJgz s)
   |> Option.orElseWith (fun () -> failwith <| sprintf "Failed to parse '%s'!" s)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let readValue (r : string) (registers : Map<RegName, int64>) : int64 =
    match Map.tryFind r registers with 
    | Some n -> n 
    | None -> 0

let writeValue (r : string) (n : int64) (registers : Map<RegName, int64>) : Map<RegName, int64> = 
    Map.add r n registers

let resolveValue (v : Value) (registers : Map<RegName, int64>) : int64 = 
    match v with 
    | Num n -> n 
    | Reg r -> 
        readValue r registers

let executeInstruction (inst : Instruction) (program : Program) : Program = 
    // printfn "Execute %A" inst
    match inst with 
    | Snd r -> 
        let n = readValue r program.registers
        if program.duet then 
            let outbox = program.outbox
            outbox.Enqueue n 
            { program with outbox = outbox; ptr = program.ptr + 1 }
        else 
            // printfn "snd %s | sound <- %d" r n
            { program with sound = n; ptr = program.ptr + 1 }
    | Rcv r -> 
        if program.duet then 
            let inbox = program.inbox 
            if inbox.Count = 0 then 
                { program with state = Waiting }
            else 
                let received = inbox.Dequeue()
                let registers = writeValue r received program.registers
                { program with registers = registers; ptr = program.ptr + 1 }
        else 
            let n = readValue r program.registers
            if n = 0 then 
                // printfn "rcv skipped"
                { program with ptr = program.ptr + 1 }
            else 
                // printfn "rcv %d" program.sound
                { program with state = Terminated }
    | Set (r, v) -> 
        let n = resolveValue v program.registers
        let registers = writeValue r n program.registers
        // printfn "set %s <- %d" r n
        { program with registers = registers; ptr = program.ptr + 1 }
    | Add (r, v) -> 
        let n = readValue r program.registers
        let m = resolveValue v program.registers
        let result = n + m
        // printfn "add %s <- %d+%d=%d" r n m result
        let registers = writeValue r result program.registers
        { program with registers = registers; ptr = program.ptr + 1 }
    | Mul (r, v) -> 
        let n = readValue r program.registers
        let m = resolveValue v program.registers
        let result = n * m
        // printfn "mul %s <- %d+%d=%d" r n m result
        let registers = writeValue r result program.registers
        { program with registers = registers; ptr = program.ptr + 1 }
    | Mod (r, v) -> 
        let n = readValue r program.registers
        let m = resolveValue v program.registers
        let result = n % m
        // printfn "mul %s <- %d+%d=%d" r n m result
        let registers = writeValue r result program.registers
        { program with registers = registers; ptr = program.ptr + 1 }
    | Jgz (v1, v2) -> 
        let x = resolveValue v1 program.registers
        let y = resolveValue v2 program.registers
        if x > 0 then 
            let ptr = program.ptr + int y
            // printfn "jgz %d %d to %d" x y ptr 
            { program with ptr = ptr}
        else 
            // printfn "jgz %d %d skipped" x y 
            { program with ptr = program.ptr + 1 }

let executeProgramStep (program : Program) = 
    match program.state with 
    | Terminated -> program 
    | Waiting -> program 
    | Running -> 
        let ptr = program.ptr 
        if ptr < 0 || ptr >= Array.length program.instructions then 
            { program with state = Terminated }
        else 
            let inst = program.instructions[ptr]
            executeInstruction inst program 

let rec executeLoop (program : Program) = 
    match program.state with 
    | Terminated -> 
        printfn "rcv %d" program.sound
    | Waiting -> failwith "?" 
    | Running -> 
        program |> executeProgramStep |> executeLoop

let rec transferMessages (sender : Program, receiver : Program) = 
    let outbox = sender.outbox
    if outbox.Count = 0 then 
        (sender, receiver)
    else 
        receiver.inbox.Enqueue(outbox.Dequeue()) 
        transferMessages (sender, receiver)

let rec executeDuetLoop (program1 : Program, program2 : Program) = 
    match (program1.state, program2.state) with 
    | Terminated, Terminated -> 
        printfn "Terminated."
    | Waiting, Waiting -> 
        printfn "Deadlocked"
    | _ -> 
        let p1 = executeProgramStep program1
        let p2 = executeProgramStep program2
        let (p11, p22) = transferMessages (p1, p2)
        let (p222, p111) = transferMessages (p22, p11)
        executeDuetLoop (p111, p222)

let initProgram duet instructions = 
    { ptr = 0  
      instructions = instructions
      state = Running
      duet = duet
      sound = 0
      registers = Map.empty
      inbox = new Queue<int64>()
      outbox = new Queue<int64>()
    }

let run fileName = 
    let lines = readLines fileName
    let instructions = lines |> Array.choose tryParse
    let program = initProgram false instructions
    executeLoop program 
    let program1 = initProgram true instructions
    let program2 = initProgram true instructions
    executeDuetLoop (program1, program2) 

run "input.txt"
