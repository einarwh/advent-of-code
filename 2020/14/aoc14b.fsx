// Advent of Code 2020. 
// Day 14: Docking Data, Part B.
// dotnet fsi aoc14b.fsx

open System.Text.RegularExpressions
open System.IO

type Bit =
    | Zero
    | One
    
type MaskBit =
    | Floating
    | Value of Bit
    
type Mask = MaskBit array

type Integer = Bit array

type Address = Bit array

type Instruction =
    | SetMask of Mask
    | WriteMemory of (Address * Integer)

type Memory = Map<Address, Integer>

let parseMask (s : string) : Mask =
    let m = Regex.Match(s, "^mask = (.+)$")
    let maskStr = m.Groups.[1].Value
    let toMaskBit ch =
        match ch with
        | '0' -> Value Zero
        | '1' -> Value One
        | _ -> Floating 
    maskStr |> Seq.map toMaskBit |> Seq.toArray

let toBits (value : int64) : Bit array =
    let (bits, _) = [0..35] |> List.fold (fun (lst, num) _ -> ((num &&& 1L) :: lst, num / 2L)) ([], value)
    bits
    |> List.map (fun n -> if n = 0L then Zero else One)
    |> List.toArray

let parseWrite (s : string) : (Address * Integer) =
    let m = Regex.Match(s, "^mem\[(\d+)\] = (\d+)$")
    let addressValue = int64 m.Groups.[1].Value
    let integerValue = int64 m.Groups.[2].Value
    let address = toBits addressValue
    let integer = toBits integerValue
    (address, integer)
    
let parseInstruction (s : string) : Instruction =
    if s.StartsWith "mask" then
        SetMask <| parseMask s 
    else
        WriteMemory <| parseWrite s 

let applyMask (mask : Mask) (address : Address) : Address list =
    let folder (bit, maskBit) bitsList =
        match maskBit with
        | Floating ->
            let lst0 = List.map (fun bits -> Zero :: bits) bitsList
            let lst1 = List.map (fun bits -> One :: bits) bitsList
            lst0 @ lst1
        | Value Zero ->
            bitsList |> List.map (fun bits -> bit :: bits)
        | Value One ->
            bitsList |> List.map (fun bits -> One :: bits)
    let zipped = Array.zip address mask
    Array.foldBack folder zipped [[]]
    |> List.map List.toArray
 
let writeMemory (address : Address) (value : Integer) memory =
    Map.add address value memory 

let runInstruction (mask : Mask, memory : Memory) (inst : Instruction) : (Mask * Memory) =
    match inst with
    | SetMask m -> (m, memory)
    | WriteMemory (address, value) ->
        let addresses : Address list = applyMask mask address
        let memory' = addresses |> List.fold (fun mem a -> mem |> writeMemory a value) memory
        (mask, memory')

let toInt64 (bits : Bit array) : int64 =
    let folder bit (result, bitval) =
        let next = 
            match bit with
            | Zero -> result
            | One -> (result ||| bitval)
        (next, 2L * bitval)
    let (result, _) = Array.foldBack folder bits (0L, 1L)
    result

let dumpValues (memory : Memory) =
    memory |> Map.toList |> List.map snd |> List.map toInt64

let run fileName =
    let initMask = parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    let initMemory = Map.empty
    let (_, memory) =
        File.ReadAllLines fileName
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map parseInstruction
        |> Array.fold runInstruction (initMask, initMemory)
    memory |> dumpValues |> List.sum |> printfn "%d" 
    0 

"input.txt" |> run 