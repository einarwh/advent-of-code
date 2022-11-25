open System.IO

let decimal (bits : int array) : int =
    let arr = [|0 .. Array.length bits - 1|] |> Array.rev |> Array.map (pown 2)
    Array.zip bits arr 
    |> Array.map (fun (a, b) -> a * b)
    |> Array.sum

let binary (line : string) : int array = 
    line |> Seq.map (fun c -> if c = '1' then 1 else 0) |> Seq.toArray
    
let gammaRate (numbers : int) (bits : int array) = 
    let half = numbers / 2
    bits |> Array.map (fun n -> if n >= half then 1 else 0)

let invert (bits : int array) = 
    bits |> Array.map (fun n -> if n = 1 then 0 else 1)

let rec count (counters : int array) (lines : string list) : int array = 
    match lines with 
    | [] -> counters 
    | h :: t -> 
        let next = 
            binary h 
            |> Array.zip counters 
            |> Array.map (fun (a, b) -> a + b)
        count next t

let run file = 
    let lines = file |> File.ReadAllLines |> List.ofArray |> List.filter (fun s -> s.Length > 0)
    let numberOfLines = List.length lines
    let binaryNumberLength = lines.[0].Length
    let counters = Array.zeroCreate binaryNumberLength
    let bits = count counters lines
    let gamma = gammaRate numberOfLines bits 
    let epsilon = invert gamma
    let powerConsumption = decimal gamma * decimal epsilon
    printfn "Power consumption: %d" powerConsumption

run "input"
