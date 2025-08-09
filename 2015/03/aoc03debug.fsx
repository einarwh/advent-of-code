// Advent of Code 2015. Day 03: Perfectly Spherical Houses in a Vacuum.
// dotnet fsi aoc03.fsx

open System
open System.IO

let move (ch : char) (x, y) = 
    match ch with 
    | '^' -> (x, y - 1)
    | '<' -> (x - 1, y)
    | 'v' -> (x, y + 1)
    | '>' -> (x + 1, y)
    | _ -> failwith "?"

let walk (s : string) = 
    let rec fn seen pos chars = 
        match chars with 
        | [] -> Set.count seen 
        | ch :: rest -> 
            let pos' = move ch pos
            let seen' = seen |> Set.add pos' 
            fn seen' pos' rest 
    let start = (0, 0)
    fn (Set.empty |> Set.add start) start (s |> Seq.toList)

let doubleWalk (s : string) = 
    let rec fn seen santaPos roboPos chars = 
        match chars with 
        | [] -> 
            // let posList = seen |> Set.toList 
            // let xs = posList |> List.map fst 
            // let ys = posList |> List.map snd 
            // let xMin, xMax = xs |> List.min, xs |> List.max 
            // let yMin, yMax = ys |> List.min, ys |> List.max 
            // printfn "%A" (xMin, yMin)
            // printfn "%A" (xMax, yMax)
            Set.count seen 
        | santaChar :: roboChar :: rest -> 
            let santaPos' = move santaChar santaPos
            let roboPos' = move roboChar roboPos
            let seen' = seen |> Set.add santaPos' |> Set.add roboPos'
            fn seen' santaPos' roboPos' rest 
        | _ -> failwith "?"
    let start = (0, 0)
    fn (Set.empty |> Set.add start) start start (s |> Seq.toList)

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    text |> walk |> printfn "%d"
    text |> doubleWalk |> printfn "%d"

run "input.txt"
