// Advent of Code 2024. Day 13
// dotnet fsi aoc13.fsx

open System
open System.IO

type Machine = 
    { ax : int 
      bx : int 
      ay : int 
      by : int 
      xGoal : int 
      yGoal : int }

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let substring (startIndex : int) (input : string) = input.Substring(startIndex)

let parseLine (s : string) : (int * int) = 
    let parts = s |> split ": " |> Array.item 1 |> split ", " |> Array.map (substring 2 >> int)
    match parts with 
    | [|x;y|] -> (x, y)
    | _ -> failwith "button ?"

let parseMachine (s : string) : Machine = 
    let toLines = split "\n" >> Array.toList
    match s |> split "\n" with 
    | [|xLine;yLine;goalLine|] -> 
        let (ax, ay) = parseLine xLine 
        let (bx, by) = parseLine yLine 
        let (gx, gy) = parseLine goalLine 
        { ax = ax; bx = bx; ay = ay; by = by; xGoal = gx; yGoal = gy }
    | _ -> failwith "machine ?"

let calculateA (m : Machine) : int = 
    (m.yGoal * m.bx - m.xGoal * m.by) / (m.bx * m.ay - m.by * m.ax)

let calculateB (a : int) (m : Machine) : int = 
    (m.xGoal - m.ax * a) / m.bx 

let calculate (m : Machine) : int option = 
    let a = m |> calculateA
    let b = m |> calculateB a 
    let x = a * m.ax + b * m.bx 
    let y = a * m.ay + b * m.by
    printfn "\nMACHINE"
    printfn "ax = %d" m.ax 
    printfn "bx = %d" m.bx 
    printfn "ay = %d" m.ay 
    printfn "by = %d" m.by 
    printfn "a = %d" a 
    printfn "b = %d" b 
    printfn "Goal = %d" m.xGoal 
    printfn "Goal = %d" m.yGoal 
    printfn "x = %d" x 
    printfn "y = %d" y 
    if x <= 100 && y <= 100 && x = m.xGoal && y = m.yGoal then 
        Some (3 * a + b) 
    else 
        None 

let run fileName = 
    let chunks = File.ReadAllText fileName |> trim |> split "\n\n" |> Array.toList
    let machines = chunks |> List.map parseMachine
    // printfn "%A" machines
    let foo = machines |> List.map calculate |> printfn "%A"
    // let machine = 
    //     { ax = 94
    //       bx = 22 
    //       ay = 34
    //       by = 67 
    //       xGoal = 8400
    //       yGoal = 5400 }
    // let a = machine |> calculateA
    // let b = machine |> calculateB a 
    // a |> printfn "%A"
    // b |> printfn "%A"
    chunks |> printfn "%A"
    0

run "sample"
