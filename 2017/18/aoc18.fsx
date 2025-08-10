// Advent of Code 2017. Day 18: Duet.
// dotnet fsi aoc18.fsx

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type RegName = string

type Value = 
    | Reg of RegName
    | Num of int

type Instruction = 
    | Snd of Value 
    | Set of RegName * Value 
    | Add of RegName * Value 
    | Mul of RegName * Value 
    | Mod of RegName * Value 
    | Rcv of Value 
    | Jgz of Value * Value 

type Program = {
    ptr : int 
    instructions : Instruction list 
    sound : int 
    registers : Map<RegName, int>
    inbox : Queue<int> 
    outbox : Queue<int>
}

let parseValue (s : string) : Value = 
    match Int32.TryParse s with
    | true, n -> Num n
    | _ -> Reg s

let tryParseSnd (line : string) : Instruction option =
    let m = Regex.Match(line, "^snd ([a-z]|\d+)$")
    if m.Success then
        let s = m.Groups.[1].Value
        let inst = Snd (parseValue s)
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
        let inst = Rcv (parseValue s)
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

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let instructions = lines |> List.choose tryParse
    instructions |> printfn "%A"
    "Not solved." |> printfn "%s"

run "input.txt"
