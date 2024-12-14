// Advent of Code 2024. Day 14
// dotnet fsi aoc14.fsx

open System
open System.IO
open System.Diagnostics

type Pos = (int * int)

type Robot = 
    { p : Pos 
      v : Pos }

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let substring (startIndex : int) (input : string) = input.Substring(startIndex)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let move width height seconds robot = 
    let (x, y) = robot.p 
    let (vx, vy) = robot.v 
    let (x', y') = ((x + (vx + width) * seconds) % width, (y + (height + vy) * seconds) % height)
    { robot with p = (x', y') }

let run fileName = 
    let xOffset = 40
    let yOffset = 20
    let width = 101
    let height = 103
    let handleLine y line = 
        line 
        |> Seq.toList 
        |> List.mapi (fun x ch -> (ch, (x + xOffset, y + yOffset)))
        |> List.choose (fun (ch, pos) -> if ch = '1' then Some pos else None)
    let random = new Random()
    let rec createRandomPosition santaWidth santaHeight = 
        let x = random.Next(0, width - 1)
        let y = random.Next(0, height - 1)
        let (xMin, xMax) = (xOffset - 1, xOffset + santaWidth + 1) 
        let (yMin, yMax) = (yOffset - 1, yOffset + santaHeight + 1)
        if (xMin <= x && x <= xMax && yMin <= y && y <= yMax) then 
            createRandomPosition santaWidth santaHeight
        else (x, y)
    let createRandomVelocity() = 
        let vxAbs = random.Next(1, 100) 
        let vyAbs = random.Next(1, 100) 
        let xf = if 0 = random.Next(2) then 1 else -1 
        let yf = if 0 = random.Next(2) then 1 else -1
        (vxAbs * xf, vyAbs * yf)
    let printRobot { p = (x, y); v = (vx, vy) } = 
        printfn "p=%d,%d v=%d,%d" x y vx vy

    let lines = readLines fileName
    let santaPositions = 
        lines 
        |> List.mapi handleLine
        |> List.concat
    let santaHeight = lines |> List.length 
    let santaWidth = lines |> List.head |> String.length 
    let posSet = santaPositions |> Set.ofList 
    let remaining = 800 - (posSet |> Set.count)
    let remainingPositions = [ 0 .. remaining - 1 ] |> List.map (fun _ -> createRandomPosition santaWidth santaHeight)
    let positions = santaPositions @ remainingPositions
    let robots = positions |> List.map (fun p -> { p = p; v = createRandomVelocity() })
    let moved = robots |> List.map (move width height 7007)
    moved |> List.iter (printRobot)

run "santa.txt"
