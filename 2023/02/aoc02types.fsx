// Advent of Code 2023. Day 1: Cube Conundrum.
// dotnet fsi aoc02.fsx

open System
open System.IO

type Sample = 
    | Red of int
    | Green of int
    | Blue of int 

type Round = Sample list

type Game = {
    Id: int
    Rounds: Round list
}

let parseSample (s : string) : Sample = 
    let ss = s.Split(" ")
    let countStr = ss[0].Trim()
    let colorStr = ss[1].Trim()
    let count = int countStr
    if colorStr = "red" then Red count
    elif colorStr = "blue" then Blue count
    elif colorStr = "green" then Green count
    else failwith "oof"

let parseRound (s : string) : Round = 
    let ss = s.Split(", ")
    ss |> Array.toList |> List.map parseSample
    
let parseGameId (s : string) : int = 
    let ss = s.Split(" ")
    ss[1].Trim() |> int

let parseGame (s : string) : Game = 
    let ss = s.Split(": ")
    let gameStr = ss[0].Trim()
    let roundsStr = ss[1].Trim()
    let gameId = parseGameId gameStr
    let rounds = roundsStr.Split("; ") |> Array.toList |> List.map parseRound
    let result = {
        Id = gameId
        Rounds = rounds
    }
    result

let isSamplePossible (sample : Sample) : bool = 
    match sample with 
    | Red n -> n <= 12 
    | Green n -> n <= 13
    | Blue n -> n <= 14 

let isRoundPossible (round : Round) : bool = 
    round |> List.forall isSamplePossible

let isGamePossible (game : Game) : bool = 
    game.Rounds |> List.forall isRoundPossible

let tryGetRed (sample : Sample) : int option = 
    match sample with 
    | Red n -> Some n 
    | _ -> None

let tryGetGreen (sample : Sample) : int option = 
    match sample with 
    | Green n -> Some n 
    | _ -> None

let tryGetBlue (sample : Sample) : int option = 
    match sample with 
    | Blue n -> Some n 
    | _ -> None

let power (game : Game) : int = 
    let reds = game.Rounds |> List.collect id |> List.choose tryGetRed |> List.max
    let greens = game.Rounds |> List.collect id |> List.choose tryGetGreen |> List.max
    let blues = game.Rounds |> List.collect id |> List.choose tryGetBlue |> List.max
    reds * greens * blues

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let lines = readLines fileName
    let games = lines |> Array.map parseGame
    let possible = 
        games 
        |> Array.filter isGamePossible
        |> Array.sumBy (fun g -> g.Id)
    printfn "%d" possible
    let powers =
        games |> Array.sumBy power
    printfn "%d" powers

run "input.txt"
