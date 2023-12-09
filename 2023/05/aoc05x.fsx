// Advent of Code 2023. Day 5: If You Give A Seed A Fertilizer
// dotnet fsi aoc05.fsx

open System.IO
open System.Text.RegularExpressions

let parseNumbers (s : string) : int64 list =
    Regex.Matches(s, "\d+") 
    |> Seq.map (fun m -> int64 m.Value)
    |> Seq.toList

let parseMappingLine (s : string) = 
    match parseNumbers s with 
    | [dst; src; range] -> 
        fun next n ->
            let i = n - src 
            if i >= 0 && i < range then dst + i else next n 
    | _ -> failwith "Wrong"

let parseMappingLine2 (s : string) = 
    let nonEmptyRange (startIndex, endIndex) = endIndex >= startIndex
    match parseNumbers s with 
    | [dst; src; range] -> 
        fun (next : (int64*int64 -> (int64*int64) list)) (inputStart, inputEnd) ->
            printfn "calling splitting function"
            let filterStart = src
            let filterEnd = src + range - 1L
            let delta = dst - src
            let intersectStart = (max inputStart filterStart)
            let intersectEnd = (min inputEnd filterEnd)
            printfn "input: (%A, %A)" inputStart inputEnd
            printfn "filter: (%A, %A)" filterStart filterEnd
            printfn "intersect: (%A, %A)" intersectStart intersectEnd
            printfn "delta = %d" delta
            if inputEnd < filterStart then 
                printfn "Nothing to do."
                next (inputStart, inputEnd)
            else 
                printfn "Split stuff."
                let moved = 
                    [ (intersectStart + delta, intersectEnd + delta) ]
                    |> List.filter nonEmptyRange
                let unmoved = 
                    [ (inputStart, intersectStart - 1L); (intersectEnd + 1L, inputEnd)]
                    |> List.filter nonEmptyRange
                let nextResults = unmoved |> List.collect next
                let res = moved @ nextResults
                printfn "moved = %A" moved
                printfn "unmoved = %A" unmoved
                res
    | _ -> failwith "Wrong"
    
let parseMap (s : string) = 
    match s.Split("\n") |> Array.toList with 
    | [] -> failwith "Nothing"
    | _ :: mappings -> 
        let functions = mappings |> List.map parseMappingLine
        List.foldBack (fun fn next -> fn next) functions id 

let parseMap2 (s : string) = 
    match s.Split("\n") |> Array.toList with 
    | [] -> failwith "Nothing"
    | _ :: mappings -> 
        let functions = mappings |> List.map parseMappingLine2
        let res = List.foldBack (fun fn next -> fn next) functions (fun x -> [x]) 
        fun items -> List.collect res items

let rec seedRanges input = 
    match input with 
    | [] -> []
    | init::range::rest ->
        (init, init + range - 1L) :: seedRanges rest 
    | _ -> failwith "Wrong"

let readChunks fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd().Split("\n\n") |> Array.toList 

let run fileName = 
    match readChunks fileName with 
    | [] -> failwith "Nothing"
    | seedChunk :: rest -> 
        let seeds = seedChunk |> parseNumbers
        // Part 1
        let fn = rest |> List.map parseMap |> List.reduce (>>)
        seeds |> List.map fn |> List.min |> printfn "%d"
        // Part 2
        // let fn2 = rest |> List.map parseMap2 |> List.reduce (>>)
        // let ranges = seeds |> seedRanges 
        // let flop = ranges |> fn2 
        // let firsts = flop |> List.map fst 
        // printfn "%A" firsts
        // printfn "."
        let fn = parseMap2 "50 98 2\n52 50 48"
        // let r = (79L, 79L + 14L - 1L)
        // printfn "r = %A" r
        // (79L, 79L + 14L - 1L) |> fn (fun x -> [x]) |> printfn "%A" 
        [(90L, 100L)] |> fn |> printfn "%A" 
        "."
        // seeds
        // |> seedRanges 
        // |> List.map (Seq.map fn >> Seq.min)
        // |> List.min
        // |> printfn "%d"
    
"sample" |> run 
