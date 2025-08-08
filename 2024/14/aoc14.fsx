// Advent of Code 2024. Day 14: Restroom Redoubt.
// dotnet fsi aoc14.fsx

open System
open System.IO
open System.Diagnostics

type Pos = (int * int)

type Robot = { p : Pos; v : Pos }

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
    positions 
    |> List.countBy id 
    |> List.sumBy snd 

let createRow robots width yRow = 
    let countedRobots = robots |> countRobotsInRow yRow 
    let robotMap : Map<int, int> = 
        countedRobots
        |> Map.ofList
    [ 0 .. (width - 1) ] 
    |> List.map (fun x -> Map.tryFind x robotMap |> Option.map string |> Option.defaultValue ".")
    |> String.concat ""

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
    let nw = robots |> toQuadrant (fun (x, y) -> x < midCol && y < midRow) |> countPositions
    let ne = robots |> toQuadrant (fun (x, y) -> x > midCol && y < midRow) |> countPositions
    let sw = robots |> toQuadrant (fun (x, y) -> x < midCol && y > midRow) |> countPositions
    let se = robots |> toQuadrant (fun (x, y) -> x > midCol && y > midRow) |> countPositions
    nw * ne * sw * se 

let neighbours (x, y) = 
  [ ((x - 1), (y - 1)); ((x, y - 1)); ((x + 1), (y - 1))
    ((x - 1), y); ((x + 1), y)
    ((x - 1), (y + 1)); ((x, y + 1)); ((x + 1), (y + 1)) ]

let rec fill (possiblePositionsLeft : Set<Pos>) (positionsToAdd : Set<Pos>) (connected : Set<Pos>) = 
    if Set.count positionsToAdd = 0 then 
        (connected, possiblePositionsLeft)
    else 
        let nextConnected = Set.union connected positionsToAdd
        let nextPositionsToAdd = 
            positionsToAdd
            |> Set.toList  
            |> List.collect neighbours 
            |> Set.ofList 
            |> Set.filter (fun p -> (Set.contains p possiblePositionsLeft))
        let possible = Set.difference possiblePositionsLeft nextPositionsToAdd
        fill possible nextPositionsToAdd nextConnected 

let fillFromPosition (positions : Set<Pos>) startPos = 
    let positionsToAdd = Set.empty |> Set.add startPos 
    fill positions positionsToAdd Set.empty

let tryFindTree (positions : Pos list)  =
    let rec loop posList = 
        match posList with 
        | [] -> None  
        | pos :: remaining -> 
            let possible = Set.ofList remaining
            let (connected, updatedPossible) = fillFromPosition possible pos 
            if Set.count connected > 100 then 
                Some connected 
            else 
                let rem = updatedPossible |> Set.toList 
                loop rem 
    loop positions

let findTree width height robots = 
    let clock = Stopwatch.StartNew()
    let rec loop seconds robots = 
        if seconds > 0 && seconds % 100 = 0 then printfn "Simulated %d 'seconds' in %d seconds" seconds ((int) clock.Elapsed.TotalSeconds)
        let positions = robots |> List.map (fun r -> r.p)
        match tryFindTree positions with 
        | Some connected -> 
            printfn "Is this a tree? After %d 'seconds'." seconds 
            visualize width height robots
        | None -> 
            let moved = simulate width height 1 robots 
            loop (seconds + 1) moved 
    loop 0 robots 

let run width height searchForTree fileName =   
    let lines = readLines fileName
    let robots = lines |> List.map parseRobot
    let moved = robots |> simulate width height 100
    moved |> calculateSafetyFactor width height |> printfn "%A"
    if searchForTree then findTree width height robots 

// run 11 7 false "sample.txt"
// run 101 103 true "input.txt"
run 101 103 true "input.txt"
