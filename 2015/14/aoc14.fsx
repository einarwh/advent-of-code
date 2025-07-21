// Advent of Code 2015. Day 14
// dotnet fsi aoc14.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Reindeer = {
    name : string
    speed : int
    flytime : int
    resttime : int
}

let parse (s : string) : Reindeer option =
    let m = Regex.Match(s, "^([A-Za-z]+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.$")
    if m.Success then
        let reindeer = {
            name = m.Groups.[1].Value
            speed = int m.Groups.[2].Value
            flytime = int m.Groups.[3].Value
            resttime = int m.Groups.[4].Value
        }
        Some reindeer
    else
        None

let makeTravelSequence (speed : int) (flytime : int) (resttime : int) = 
    let flySeq = [0 .. flytime - 1] |> List.map (fun _ -> speed)
    let restSeq = [0 .. resttime - 1] |> List.map (fun _ -> 0)
    let finiteSeq = flySeq @ restSeq
    Seq.initInfinite (fun i -> finiteSeq.[i % List.length finiteSeq])

let getDistances (timeLimit : int) (reindeer : Reindeer) =
    let travelSeq = makeTravelSequence reindeer.speed reindeer.flytime reindeer.resttime
    travelSeq |> Seq.take timeLimit |> Seq.scan (+) 0 |> Seq.tail |> Seq.toArray

let race (reindeerDistances : int array array) = 
    let reindeerCount = reindeerDistances.Length
    let totalSeconds = reindeerDistances[0].Length
    let rec loop results (seconds : int) = 
        if seconds < totalSeconds then 
            let distances = [|0 .. reindeerCount - 1|] |> Array.map (fun reindeer -> reindeerDistances[reindeer][seconds])
            let maxDist = Array.max distances
            let scores = distances |> Array.map (fun d -> if d = maxDist then 1 else 0)
            loop (scores :: results) (seconds + 1)
        else 
            results |> List.toArray
    let results = loop [] 0
    [|0 .. reindeerCount - 1|] |> Array.map (fun reindeer -> results |> Array.sumBy (fun result -> result[reindeer])) |> Array.max

let readLines =
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run timeLimit fileName =
    let lines = readLines fileName
    let reindeerList = lines |> List.choose parse
    let reindeerDistances = reindeerList |> List.map (getDistances timeLimit) |> List.toArray
    reindeerDistances |> Array.map Array.last |> Array.max |> printfn "%d"
    reindeerDistances |> race |> printfn "%d"

// run 1000 "sample.txt"
run 2503 "input.txt"
