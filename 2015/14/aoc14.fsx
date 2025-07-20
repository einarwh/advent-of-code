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

let travel (timeLimit : int) (reindeer : Reindeer) = 
    let rec loop elapsed distance = 
        if elapsed < timeLimit then 
            let flytime = min reindeer.flytime (timeLimit - elapsed)
            let elapsedAfterFlying = elapsed + flytime
            let distanceAfterFlying = distance + flytime * reindeer.speed 
            loop (elapsedAfterFlying + reindeer.resttime) distanceAfterFlying
        else 
            distance 
    loop 0 0 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    lines |> printfn "%A"
    let reindeerList = lines |> List.choose parse
    printfn "%A" reindeerList
    reindeerList |> List.map (travel 1000) |> printfn "%A"

run "sample.txt"
