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

let createRow robots width yRow = 
    let countedRobots = 
        robots 
        |> List.map (fun r -> r.p) 
        |> List.choose (fun (x, y) -> if y = yRow then Some x else None) 
        |> List.countBy id 
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

let run fileName = 
    // width: 11
    // height: 7
    let lines = readLines fileName
    // let robots = lines |> List.map parseRobot
    // visualize 11 7 robots 
    // // robots |> printfn "%A"
    // printfn ""
    // let moved = robots |> simulate 11 7 100
    // moved |> List.map (fun r -> r.p) |> List.iter (printfn "%A")
    // visualize 11 7 moved 
    // // moved |> List.iter (printfn "%A")
    // let robot = { p=(2,4); v=(2,-3) }
    let robot = parseRobot "p=2,4 v=2,-3"
    let after seconds robots = 
        let moved = robots |> simulate 11 7 seconds
        moved |> List.map (fun r -> r.p) |> List.iter (printfn "%A")
        moved |> visualize 11 7 
    // let after seconds [ robot  = robot |> move 11 7 seconds |> visualize 11 7 
    printfn "%A" robot
    printfn "\nAfter 0 seconds"
    [ robot ] |> after 0
    printfn "\nAfter 1 seconds"
    [ robot ] |> after 1
    printfn "\nAfter 2 seconds"
    [ robot ] |> after 2
    printfn "\nAfter 3 seconds"
    [ robot ] |> after 3
    printfn "\nAfter 4 seconds"
    [ robot ] |> after 4
    printfn "\nAfter 5 seconds"
    [ robot ] |> after 5
    0 

run "sample"
