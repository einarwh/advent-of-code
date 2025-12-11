// Advent of Code 2025. Day 11: Reactor.
// dotnet fsi aoc11.fsx

open System
open System.IO

let parse (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> a, b.Split " "
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let solve start dac fft flow = 
    let rec loop device dac fft lookup = 
        let key = $"{device}-{dac}-{fft}"
        if device = "out" then 
            lookup, if dac && fft then 1L else 0L
        else
            match Map.tryFind key lookup with 
            | Some count -> lookup, count
            | None -> 
                let devices = Map.find device flow 
                let folder (m, acc) d = 
                    let m', c = m |> loop d (dac || d = "dac") (fft || d = "fft")
                    m', acc + c
                let lookup', count = Array.fold folder (lookup, 0) devices 
                lookup' |> Map.add key count, count
    Map.empty |> loop start dac fft

let createColorMap devices lookupKeys = 
    let chooseColor d = 
        if d = "out" then 
            Some "black"
        else if d = "srv" then 
            Some "black"
        else if d = "you" then 
            Some "black"
        else if d = "fft" then 
            Some "black"
        else if d = "dac" then 
            Some "black"
        else if lookupKeys |> List.contains $"{d}-True-True" then 
            Some "blue"
        else if lookupKeys |> List.contains $"{d}-True-False" then 
            Some "green"
        else if lookupKeys |> List.contains $"{d}-False-True" then 
            Some "purple"
        else 
            None

    devices |> List.choose (fun d -> chooseColor d |> Option.map (fun c -> (d, c))) |> Map.ofList

let toNodeDeclaration colorMap n =
    match colorMap |> Map.tryFind n with 
    | Some color -> 
        $"  {n} [style=filled,fillcolor={color},color={color}];"
    | None ->  
        $"  {n} [];"

let toEdgeDeclaration (src, tgt) = 
    $"  {src} -> {tgt};"

let run fileName = 
    let lines = readLines fileName
    let tuples = lines |> Array.map parse
    let flow = tuples |> Map.ofArray
    // solve "you" true true flow |> printfn "%d"
    let (lookup, count) = solve "svr" false false flow 
    let devices = Map.keys flow |> Seq.toList
    let lookupKeys = Map.keys lookup |> Seq.toList
    // printfn "%A" lookupKeys
    let colorMap = createColorMap devices lookupKeys 
    let sources = tuples |> Array.map fst |> Set.ofArray
    let targets = tuples |> Array.collect snd |> Set.ofArray 
    let nodes = Set.union sources targets |> Set.toArray
    let edges = tuples |> Array.collect (fun (a, bs) -> bs |> Array.map (fun b -> (a, b)))
    
    let nodeDeclarations = nodes |> Array.map (toNodeDeclaration colorMap)
    let edgeDeclarations = edges |> Array.map toEdgeDeclaration 
    let prefix = [| "digraph G {" |]
    let postfix = [| "}" |]
    let dotLines = Array.concat [ prefix; nodeDeclarations; edgeDeclarations; postfix ]
    let s = dotLines |> String.concat "\n"
    printfn "%s" s 
    0

run "input.txt"
