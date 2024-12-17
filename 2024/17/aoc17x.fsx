// Advent of Code 2024. Day 17: 
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

let readOpcode computer : int option =
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

let division computer =
    let operand = combo computer
    // printfn "operand %d" operand
    let numerator = computer.regA
    let denominator = int64 (Math.Pow(2.0, double operand))
    let result = numerator / denominator
    // printfn "division %d %d" numerator denominator
    result

let xor v1 v2 : int64 = v1 ^^^ v2

let modulo v : int64 = v % 8L

let adv (computer : Computer) : Computer =
    // printfn "adv"
    let result = division computer
    computer
    |> writeA result
    |> nextInstruction

let bdv computer =
    // printfn "adv"
    let result = division computer
    computer
    |> writeB result
    |> nextInstruction

let cdv computer =
    // printfn "cdv"
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

let rec execute (computer : Computer) =
    match readOpcode computer with
    | None ->
        computer.outputs |> List.rev |> List.toArray 
    | Some opcode ->
        // printfn "execute, read opcode %d at %d" opcode computer.pointer
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

let findShifts computer = 
    let len = Array.length computer.program
    let rec loop shifts c = 
        // printfn "loop %d shifts" shifts
        let n = 1L <<< shifts
        // printfn "n = %d" n 
        let p = execute { computer with regA = n }
        // printProgram p
        if Array.length p < len then 
            loop (shifts + 1) c 
        else 
            shifts
    loop 1 computer

let bruteQuine computer = 
    // printfn "\nbrute force quine"
    let rec loop a = 
        let c = { computer with regA = a }
        let p = execute c 
        printf "A: %A - " a 
        printProgram p 
        if p = computer.program then 
            a 
        else 
            loop (a + 1L)
    loop 0L 

let quine computer = 
    printfn "\nquine"
    let len = computer.program |> Array.length 
    printfn "program "
    printProgram computer.program
    let shifts = findShifts computer 

    let rec loop index value = 
        printfn ""
        printfn "loop index:%d a:%d" index value
        let programIndex = len - (index + 1)
        if programIndex < 0 then 
            value 
        else 
            let sft = shifts - index
            let targetNumber = computer.program.[programIndex]
            printfn "target number %d" targetNumber
            let exec ix = 
                let a = value + ix <<< sft
                let c = { computer with regA = a }
                (a, execute c)
            [0L .. 8L] 
                |> List.map (fun i -> exec i)
                |> List.iter (printfn "%A")
            let a = 
                [0L .. 8L] 
                |> List.map (fun i -> exec i)
                |> List.filter (fun (a, program) -> program |> Array.length >= len)
                |> List.find (fun (a, program) -> program.[programIndex] = targetNumber)
                |> fst 
            a |> printfn "%d"
            loop (index + 1) a 

    printfn "%d" shifts 
    loop 0 0 
    0

let run fileName =
    let text = File.ReadAllText fileName |> trim |> split "\n"
    let computer = parseComputer text
    // computer |> printfn "%A"
    // execute computer |> Array.map string |> String.concat "," |> printfn "%s"
    // let startValue = 35184372000000L
    let startValue = 281474980000000L
    // let shifts = findShifts computer
    // printfn "shifts %d" shifts
    // (1L <<< shifts) |> printfn "%d"
    // (2L <<< shifts) |> printfn "%d"
    // (3L <<< shifts) |> printfn "%d"
    // (4L <<< shifts) |> printfn "%d"
    // (5L <<< shifts) |> printfn "%d"
    // (6L <<< shifts) |> printfn "%d"
    // (7L <<< shifts) |> printfn "%d"
    // (8L <<< shifts) |> printfn "%d"

    // execute { computer with regA = (1L <<< shifts) } |> printProgram 
    // execute { computer with regA = (2L <<< shifts) } |> printProgram 
    // execute { computer with regA = (3L <<< shifts) } |> printProgram 
    // execute { computer with regA = (4L <<< shifts) } |> printProgram 
    // execute { computer with regA = (5L <<< shifts) } |> printProgram 
    // execute { computer with regA = (6L <<< shifts) } |> printProgram 
    // execute { computer with regA = (7L <<< shifts) } |> printProgram 
    // execute { computer with regA = (0 <<< shifts) } |> printProgram 
    
    printProgram computer.program

    let len = computer.program |> Array.length
    let len64 = int64 len 
    printfn "program length %d" len
    let rec pow x y = if y < 1L then 1L else x * pow x (y - 1L)
    
    let a0 = pow 8L (len64 - 1L)
    printfn "a0 %d" a0 
    let target0 = computer.program.[len - 1]
    printfn "target0: %d" target0
    let offset0 = pow 8L (len64 - 1L)
    printfn "offset0: %A" offset0
    let p0 = execute { computer with regA = a0 + 0L * offset0 }
    printfn "Number: %d" p0.[len - 1]
    let p1 = execute { computer with regA = a0 + 1L * offset0 }
    printfn "Number: %d" p1.[len - 1]
    let p2 = execute { computer with regA = a0 + 2L * offset0 }
    printfn "Number: %d" p2.[len - 1]

    let a1 = a0 + 2L * offset0
    printfn "%A" a1
    let target1 = computer.program.[len - 2]
    printfn "target1: %d" target1
    let offset1 = pow 8L (len64 - 2L)
    printfn "offset1 %A" offset1 
    let p20 = execute { computer with regA = a1 + 0L * offset1 }
    printfn "Number: %d" p20.[len - 2]

    let a2 = a1 + 0L * offset1 
    printfn "%A" a2
    let target2 = computer.program.[len - 3]
    printfn "target2: %d" target2
    let offset2 = pow 8L (len64 - 3L)
    printfn "offset2 %A" offset2 
    let p200 = execute { computer with regA = a2 + 0L * offset2 }
    printfn "Number: %d" p200.[len - 3]
    let p201 = execute { computer with regA = a2 + 1L * offset2 }
    printfn "Number: %d" p201.[len - 3]
    let p202 = execute { computer with regA = a2 + 2L * offset2 }
    printfn "Number: %d" p202.[len - 3]
    let p203 = execute { computer with regA = a2 + 3L * offset2 }
    printfn "Number: %d" p203.[len - 3]
    let p204 = execute { computer with regA = a2 + 4L * offset2 }
    printfn "Number: %d" p204.[len - 3]
    let p205 = execute { computer with regA = a2 + 5L * offset2 }
    printfn "Number: %d" p205.[len - 3]



    

    // bruteQuine computer 
    // quine startValue computer |> printfn "%d" 
    0 

run "input"
