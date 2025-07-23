// Advent of Code 2015. Day 07: Some Assembly Required.
// dotnet fsi aoc07.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Wire = 
    | Name of string 
    | Signal of int 

type Gate =
    | Direct of Wire
    | And of Wire * Wire
    | Or of Wire * Wire
    | Not of Wire
    | Lshift of string * int
    | Rshift of string * int

// type Element = Wire * Gate

let parseInput (str : string) = 
    match Int32.TryParse str with
    | true, n -> Signal n
    | _ -> Name str 

let tryParseDirect (s : String) : (Wire * Gate) option =
    let m = Regex.Match(s, "^(\d+|[a-z]+) -> ([a-z]+)$")
    if m.Success then
        let input = parseInput m.Groups.[1].Value
        let output = Name m.Groups.[2].Value
        Some (output, Direct input)
    else
        None

let tryParseAnd (s : String) : (Wire * Gate) option =
    let m = Regex.Match(s, "^(\d+|[a-z]+) AND (\d+|[a-z]+) -> ([a-z]+)$")
    if m.Success then
        let input1 = parseInput m.Groups.[1].Value
        let input2 = parseInput m.Groups.[2].Value
        let output = Name m.Groups.[3].Value
        Some (output, And (input1, input2))
    else
        None

let tryParseOr (s : String) : (Wire * Gate) option =
    let m = Regex.Match(s, "^(\d+|[a-z]+) OR (\d+|[a-z]+) -> ([a-z]+)$")
    if m.Success then
        let input1 = parseInput m.Groups.[1].Value
        let input2 = parseInput m.Groups.[2].Value
        let output = Name m.Groups.[3].Value
        Some (output, Or (input1, input2))
    else
        None

let tryParseNot (s : String) : (Wire * Gate) option =
    let m = Regex.Match(s, "^NOT (\d+|[a-z]+) -> ([a-z]+)$")
    if m.Success then
        let input = parseInput m.Groups.[1].Value
        let output = Name m.Groups.[2].Value
        Some (output, Not input)
    else
        None

let tryParseLshift (s : String) : (Wire * Gate) option =
    let m = Regex.Match(s, "^([a-z]+) LSHIFT (\d+) -> ([a-z]+)$")
    if m.Success then
        let input = m.Groups.[1].Value
        let shift = int <| m.Groups.[2].Value
        let output = Name m.Groups.[3].Value
        Some (output, Lshift (input, shift))
    else
        None

let tryParseRshift (s : String) : (Wire * Gate) option =
    let m = Regex.Match(s, "^([a-z]+) RSHIFT (\d+) -> ([a-z]+)$")
    if m.Success then
        let input = m.Groups.[1].Value
        let shift = int <| m.Groups.[2].Value
        let output = Name m.Groups.[3].Value
        Some (output, Rshift (input, shift))
    else
        None

let tryParse (s : String) : (Wire * Gate) option =
   s
   |> tryParseDirect
   |> Option.orElseWith (fun () -> tryParseAnd s)
   |> Option.orElseWith (fun () -> tryParseOr s)
   |> Option.orElseWith (fun () -> tryParseNot s)
   |> Option.orElseWith (fun () -> tryParseLshift s)
   |> Option.orElseWith (fun () -> tryParseRshift s)
   |> Option.orElseWith (fun () -> failwith <| sprintf "Failed to parse '%s'!" s)

let readLines =
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let resolveInput (signalMap : Map<string, int>) (input : Wire) = 
    match input with 
    | Name n -> 
        match Map.tryFind n signalMap with 
        | Some s -> Signal s 
        | None -> input 
    | _ -> input

let solve (elements : (Wire * Gate) list) =
    let tryResolve (signalMap : Map<string, int>) (wire : Wire, gate : Gate) : Wire * Gate =
        match gate with
        | Direct input -> 
            let input' = input |> resolveInput signalMap 
            wire, Direct input'
        | And (input1, input2) ->
            let input1' = input1 |> resolveInput signalMap
            let input2' = input2 |> resolveInput signalMap
            match input1', input2' with 
            | Signal sg1, Signal sg2 -> 
                wire, Direct (Signal (sg1 &&& sg2))
            | _ -> 
                wire, And (input1', input2')
        | Or (input1, input2) ->
            let input1' = input1 |> resolveInput signalMap
            let input2' = input2 |> resolveInput signalMap
            match input1', input2' with 
            | Signal sg1, Signal sg2 -> 
                wire, Direct (Signal (sg1 ||| sg2))
            | _ -> 
                wire, Or (input1', input2')
        | Not input -> 
            let input' = input |> resolveInput signalMap
            match input with 
            | Signal sg -> 
                wire, Direct (Signal ~~~sg)
            | _ -> 
                wire, Not input'
        | Lshift (name, shift) ->
            match Map.tryFind name signalMap with
            | Some sg -> 
                wire, Direct (Signal (sg <<< shift))
            | _ -> 
                wire, gate
        | Rshift (name, shift) ->
            match Map.tryFind name signalMap with
            | Some sg -> 
                wire, Direct (Signal (sg >>> shift))
            | _ -> 
                wire, gate
    let isDirectSignal e =
        match e with
        | _, Direct (Signal _) -> true
        | _ -> false
    let toDirectSignal (w : Wire, g : Gate) : (string * int) option =
        match w, g with | Name n, Direct (Signal sg) -> Some (n, sg) | _ -> None
    let (signalElements, otherElements) = elements |> List.partition isDirectSignal
    let rec loop (signals : (string * int) list) (unresolved : (Wire * Gate) list) =
        // printfn "loop with signals %d unresolved %d" signals.Length unresolved.Length
        if List.isEmpty unresolved then 
            signals |> List.find (fun (n, v) -> n = "a") |> snd
        else
            let signalMap = signals |> Map.ofList
            let result : (Wire * Gate) list = unresolved |> List.map (tryResolve signalMap)
            let (newSignalElements, newUnresolved) = result |> List.partition isDirectSignal
            let newSignals = newSignalElements |> List.choose toDirectSignal
            let oldSignals = signalMap |> Map.toList
            loop (oldSignals @ newSignals) newUnresolved
    let signals = signalElements |> List.choose toDirectSignal
    loop signals otherElements

let run fileName =
    let lines = readLines fileName
    let elements = lines |> List.choose tryParse
    let a = elements |> solve 
    let replaceB (w, g) = 
        match w with 
        | Name n when n = "b" -> w, Direct (Signal a)
        | _ -> w, g
    let a' = elements |> List.map replaceB |> solve 
    a' |> printfn "%d"

run "input.txt"
