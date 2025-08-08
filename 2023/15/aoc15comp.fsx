// Advent of Code 2023. Day 15: Lens Library.
// dotnet fsi aoc15.fsx

open System.IO

let readText fileName = 
    let text = File.ReadAllText fileName 
    text.TrimEnd()

let getHash s = 
    let folder current ch = ((current + int ch) * 17) % 256
    s |> Seq.toList |> List.fold folder 0

let parseOp (s : string) = 
    let contains label = 
        List.exists (fun (l, _) -> l = label)
    let remove label = 
        List.filter (fun (l, _) -> l <> label)
    let replace label focal = 
        List.map (fun (l, f) -> if l = label then (l, focal) else (l, f))
    let append label focal = 
        (@) [label, focal]
    if s.EndsWith('-') then 
        let label = s.Substring(0, s.Length - 1)
        fun hashmap -> 
            let tryRemove = Option.map (remove label)
            let hash = getHash label 
            hashmap |> Map.change hash tryRemove
    else 
        let ss = s.Split("=")
        let label = ss[0] 
        let focal = int ss[1]
        fun hashmap -> 
            let insert maybe = 
                match maybe with 
                | Some lenses ->
                    if lenses |> contains label then 
                        lenses |> replace label focal |> Some
                    else 
                        lenses |> append label focal |> Some 
                | None -> Some [label, focal]
            let hash = getHash label 
            hashmap |> Map.change hash insert 

let compose (s : string) = 
    s.Split(",") |> Array.map parseOp |> Array.reduce (>>)

let part1 (input : string) = 
    input.Split(",") |> Array.sumBy getHash

let part2 input = 
    let calculate (boxNo, lenses) = 
        lenses |> List.map snd |> List.mapi (fun i focal -> (boxNo + 1) * (i + 1) * focal)
    Map.empty
    |> compose input
    |> Map.toList 
    |> List.collect calculate
    |> List.sum 

let run fileName =
    let input = readText fileName
    input |> part1 |> printfn "%d"
    input |> part2 |> printfn "%d"
    
"input.txt" |> run
