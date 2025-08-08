// Advent of Code 2021. Day 3: Binary Diagnostic.
// dotnet fsi aoc03.fsx

open System.IO

let decimal (bits : int array) : int =
    let arr = [|0 .. Array.length bits - 1|] |> Array.rev |> Array.map (pown 2)
    Array.zip bits arr 
    |> Array.map (fun (a, b) -> a * b)
    |> Array.sum

let binaryArray (line : string) : int array = 
    line |> Seq.map (fun c -> if c = '1' then 1 else 0) |> Seq.toArray

let gammaRate (numberOfNumbers : int) (bits : int array) = 
    let half = numberOfNumbers / 2
    bits |> Array.map (fun n -> if n >= half then 1 else 0)

let invert (bits : int array) = 
    bits |> Array.map (fun n -> if n = 1 then 0 else 1)

let rec count (counters : int array) (numbers : int array list) : int array = 
    match numbers with 
    | [] -> counters 
    | h :: t -> 
        let next = 
            h 
            |> Array.zip counters 
            |> Array.map (fun (a, b) -> a + b)
        count next t

let rec findRating (selector :int -> int -> int) (index : int) (numbers : int array list) : int array = 
    match numbers with 
    | [] -> failwith "No numbers left, not found."
    | [x] -> x
    | h :: t -> 
        let digitsAtIndex = numbers |> List.map (fun n -> n.[index])
        let ones = digitsAtIndex |> List.filter (fun d -> d = 1) |> List.length
        let zeros = (digitsAtIndex |> List.length) - ones
        let criteria = selector ones zeros
        let remaining = numbers |> List.filter (fun n -> n.[index] = criteria)
        findRating selector (index + 1) remaining

let oxygenGeneratorRatingCriteria ones zeros = 
    if ones >= zeros then 1 else 0

let findOxygenGeneratorRating = findRating oxygenGeneratorRatingCriteria 0 

let co2ScrubberRatingCriteria ones zeros = 
    if zeros <= ones then 0 else 1

let findCo2ScrubberRating = findRating co2ScrubberRatingCriteria 0 

let run file = 
    let lines = file |> File.ReadAllLines |> List.ofArray |> List.filter (fun s -> s.Length > 0)
    let binaryNumbers = lines |> List.map binaryArray
    let numberOfNumbers = List.length binaryNumbers
    let binaryNumberLength = binaryNumbers.[0].Length
    let counters = Array.zeroCreate binaryNumberLength
    let bits = count counters binaryNumbers
    let gamma = gammaRate numberOfNumbers bits 
    let epsilon = invert gamma
    let powerConsumption = decimal gamma * decimal epsilon
    printfn "Power consumption: %d" powerConsumption
    let oxygenGeneratorRating = findOxygenGeneratorRating binaryNumbers
    let co2ScrubberRating = findCo2ScrubberRating binaryNumbers
    let lifeSupportRating = decimal oxygenGeneratorRating * decimal co2ScrubberRating
    printfn "Life support rating: %d" lifeSupportRating

run "input.txt"
