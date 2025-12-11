// Advent of Code 2025. Day 11: Reactor.
// dotnet fsi aoc11dot.fsx

open System
open System.IO

let parse (s : string) = 
    match s.Split ": " with 
    | [|a; b|] -> a, b.Split " "
    | _ -> failwith "?"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let reaches start goal flow = 
    let rec loop device lookup = 
        let key = $"{device}"
        if device = goal then 
            // let lookup' = lookup |> Map.add key true 
            lookup, true
        else
            match Map.tryFind key lookup with 
            | Some result -> lookup, result
            | None -> 
                match Map.tryFind device flow with 
                | Some devices -> 
                    let folder (m, acc) d = 
                        let m', c = m |> loop d 
                        m', acc || c
                    let lookup', included = Array.fold folder (lookup, false) devices 
                    lookup' |> Map.add key included, included
                | None -> (lookup, false)
    let lookup = Map.empty |> loop start |> fst 
    lookup |> Map.toList |> List.choose (fun (d, included) -> if included then Some d else None)

let createColorMap devices reachesFft = 
    let chooseColor d = 
        if d = "out" then 
            Some "black"
        else if d = "svr" then 
            Some "black"
        // else if d = "you" then 
        //     Some "red"
        else if d = "fft" then 
            Some "black"
        else if d = "dac" then 
            Some "black"
        else if List.contains d reachesFft then 
            Some "lightgrey"
        else 
            None

    // devices |> List.iter (fun d -> printfn "DEVICE %s" d)
    devices |> List.choose (fun d -> chooseColor d |> Option.map (fun c -> (d, c))) |> Map.ofList

let toNodeDeclaration colorMap n =
    match colorMap |> Map.tryFind n with 
    | Some fill -> 
        let fontcolor = if fill = "black" then "white" else "black"
        $"  {n} [style=filled,fillcolor={fill},color=black,fontcolor={fontcolor}];"
    | None ->  
        $"  {n} [];"

let toEdgeDeclaration (src, tgt) = 
    $"  {src} -> {tgt};"

let run fileName = 
    let lines = readLines fileName
    let tuples = lines |> Array.map parse
    let flow = tuples |> Map.ofArray
    let reachesFft = reaches "svr" "fft" flow
    let reachesDac = reaches "fft" "dac" flow 
    let reachesOut = reaches "dac" "out" flow
    let included = List.concat [ reachesFft; reachesDac; reachesOut ]
    let sources = tuples |> Array.map fst |> Set.ofArray
    let targets = tuples |> Array.collect snd |> Set.ofArray 
    let nodes = Set.union sources targets |> Set.toArray
    let colorMap = createColorMap (nodes |> Array.toList) included 
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
