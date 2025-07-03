// Advent of Code 2024. Day 22: Monkey Market. Part 1.
// dotnet fsi aoc22.fsx

open System.IO

let mix a b = a ^^^ b 
let prune x = x &&& 16777215
let step1 x = prune (mix (x <<< 6) x)
let step2 x = prune (mix (x >>> 5) x) 
let step3 x = prune (mix (x <<< 11) x)
let next = step1 >> step2 >> step3 
let infiniteSecretNumbers =  Seq.unfold (fun n -> Some (n, next n))
let secretNumbers2k x = x |> infiniteSecretNumbers |> Seq.take 2001 |> Seq.toList

let rec safeZip lst1 lst2 = 
    match lst1, lst2 with 
    | [], _ -> []
    | _, [] -> []
    | h1::r1, h2::r2 -> (h1, h2) :: safeZip r1 r2 

let toLookup items = 
    let rec fn map items = 
        match items with 
        | [] -> map 
        | (k, v) :: rest -> 
            if Map.containsKey k map then 
                fn map rest 
            else 
                fn (Map.add k v map) rest 
    fn Map.empty items 

let lookup key map = 
    Map.tryFind key map |> Option.defaultValue 0 

let toCode lst = 
    let rec fn code lst = 
        match lst with 
        | [] -> code 
        | h :: rest -> 
            fn ((h + 9) + (code <<< 8)) rest 
    fn 0 lst 

let createMap secretNumbers = 
    let prices = secretNumbers |> List.map (fun n -> n % 10)
    let diffs = prices |> List.tail |> safeZip prices |> List.map (fun (prev, next) -> next - prev)
    let wind = diffs |> List.windowed 4 |> List.map toCode 
    let zap = prices |> List.skip 4 |> List.zip wind
    zap |> toLookup

let rec collectKeys keySet maps = 
    printfn "collect..."
    match maps with 
    | [] -> keySet |> Set.toList 
    | m :: rest -> 
        let keys = Map.keys m |> Seq.toList 
        printfn "keys! %A" keys
        let keySet' = keySet |> Set.union (Set.ofSeq keys) 
        printfn "keySet' %A" keySet'
        collectKeys keySet' rest  

// let summarize keys maps = 


let run fileName =
    let lines = File.ReadAllLines fileName
    let buyers = lines |> Seq.toList |> List.map int
    printfn "%A" buyers
    let secretNumbersList = buyers |> List.map secretNumbers2k
    let lastSecretList = secretNumbersList |> List.map List.last
    printfn "%A" (secretNumbersList |> List.map List.last)
    lastSecretList |> List.map int64 |> List.sum |> printfn "%d"
    let maps = secretNumbersList |> List.map createMap
    printfn "MAPPPS %A" maps
    let keys = collectKeys Set.empty maps 
    // keys |> List.map (fun key -> maps |> List.map (fun m -> lookup key m)) |> printfn "distinct %A"
    printfn "KEEEEYYYS %A" keys
    ()

run "sample"
