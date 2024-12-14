// Advent of Code 2024. Day 14
// dotnet fsi aoc14.fsx

open System
open System.IO

type Pos = (int * int)

type Robot = 
    { p : Pos 
      v : Pos }

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let substring (startIndex : int) (input : string) = input.Substring(startIndex)

let parsePos s = 
    match s |> split "=" |> Array.item 1 |> split "," |> Array.map int with  
    | [|x;y|] -> (x, y) 
    | _ -> failwith "pos ?"

let parseRobot s = 
    match s |> split " " with 
    | [|pStr;vStr|] -> 
        { p = parsePos pStr; v = parsePos vStr }
    | _ -> failwith "robot ?"

let move width height seconds robot = 
    let (x, y) = robot.p 
    let (vx, vy) = robot.v 
    let (x', y') = ((x + (vx + width) * seconds) % width, (y + (height + vy) * seconds) % height)
    { robot with p = (x', y') }

let simulate width height seconds (robots : Robot list) = 
    robots |> List.map (move width height seconds)

let countRobotsInRow yRow robots = 
    robots 
    |> List.map (fun r -> r.p) 
    |> List.choose (fun (x, y) -> if y = yRow then Some x else None) 
    |> List.countBy id 

let countPositions (positions : Pos list) : int = 
    let foo = 
        positions 
        |> List.countBy id 
        |> List.sumBy snd 
    foo

let createRow robots width yRow = 
    let countedRobots = robots |> countRobotsInRow yRow 
    let robotMap : Map<int, int> = 
        countedRobots
        |> Map.ofList
    [ 0 .. (width - 1) ] 
    |> List.map (fun x -> Map.tryFind x robotMap |> Option.map string |> Option.defaultValue ".")
    |> String.concat ""
    // []

let visualize width height robots = 
    [ 0 .. (height - 1) ] |> List.map (createRow robots width) |> String.concat "\n" |> printfn "%s"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let toQuadrant predicate robots = 
    robots |> List.map (fun r -> r.p) |> List.filter predicate

let calculateSafetyFactor width height robots = 
    let midRow = height / 2
    let midCol = width / 2
    let nw = toQuadrant (fun (x, y) -> x < midCol && y < midRow) |> countPositions
    let ne = toQuadrant (fun (x, y) -> x > midCol && y < midRow) |> countPositions
    let sw = toQuadrant (fun (x, y) -> x < midCol && y > midRow) |> countPositions
    let se = toQuadrant (fun (x, y) -> x > midCol && y > midRow) |> countPositions
    nw * ne * sw * se 

let run width height fileName = 
    let lines = readLines fileName
    let robots = lines |> List.map parseRobot
    let moved = robots |> simulate width height 100
    moved |> calculateSafetyFactor width height |> printfn "%A"

run 11 7 "sample"
run 101 103 "input"
