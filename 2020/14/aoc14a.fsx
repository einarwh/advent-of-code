// Advent of Code 2020. 
// Day 14: Docking Data, Part A.
// dotnet fsi aoc14a.fsx

open System.IO
open System.Text.RegularExpressions

type Integer = int64
type Address = int 

type Mask = (Integer * Integer)

type Instruction =
    | SetMask of Mask
    | WriteMemory of (Address * Integer)

type Memory = Map<Address, Integer>

let parseMask (s : string) : Mask =
    let folder target ch (bitno, mask) =
        (2L*bitno, if ch = target then mask ||| bitno else mask)
    let on = Seq.foldBack (folder '1') s (1L, 0L) |> snd 
    let off = Seq.foldBack (folder '0') s (1L, 0L) |> snd 
    (on, off ^^^ 34359738367L)

let parseWrite (s : string) : (Address * Integer) =
    let m = Regex.Match(s, "^mem\[(\d+)\] = (\d+)$")
    let address = int m.Groups.[1].Value
    let integer = int64 m.Groups.[2].Value
    (address, integer)
    
let parseInstruction (s : string) : Instruction =
    if s.StartsWith "mask" then
        SetMask <| parseMask s 
    else
        WriteMemory <| parseWrite s 
    
let applyMask (on : Integer, off : Integer) (value : Integer) : Integer =
    value &&& off ||| on

let writeMemory = Map.add

let runInstruction (mask : Mask, memory : Memory) (inst : Instruction) : (Mask * Memory) =
    match inst with
    | SetMask m -> (m, memory)
    | WriteMemory (address, value) ->
        let masked = applyMask mask value
        (mask, memory |> writeMemory address masked)
        
let dumpValues (memory : Memory) =
    memory |> Map.toList |> List.map snd 
    
let run fileName =
    let initMask = parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    let initMemory = Map.empty
    let endState =
        File.ReadAllLines fileName
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map parseInstruction
        |> Array.fold runInstruction (initMask, initMemory)
    let sum = endState |> snd |> dumpValues |> List.sum 
    printfn "%d" sum
    0 

"input.txt" |> run 