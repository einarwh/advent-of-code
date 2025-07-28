// Advent of Code 2022. Day 2: Rock Paper Scissors. 
// dotnet fsi aoc02x.fsx

open System.IO

type Play = 
    | Rock 
    | Paper 
    | Scissors

type Goal = 
    | Win 
    | Draw 
    | Lose

let interpretElf s = 
    match s with 
    | "A" -> Rock 
    | "B" -> Paper 
    | "C" -> Scissors
    | _ -> failwith "boo"

let interpretPlay s = 
    match s with 
    | "X" -> Rock 
    | "Y" -> Paper 
    | "Z" -> Scissors
    | _ -> failwith "boo"

let interpretGoal s = 
    match s with 
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "boo"

let parse1 (line : string) : (Play * Goal) = 
    let ss = line.Split(" ")
    match (interpretElf ss.[0], interpretPlay ss.[1]) with 
    | Rock, Rock -> Rock, Draw
    | Rock, Paper -> Paper, Win
    | Rock, Scissors -> Scissors, Lose
    | Paper, Rock -> Rock, Lose
    | Paper, Paper -> Paper, Draw
    | Paper, Scissors -> Scissors, Win
    | Scissors, Rock -> Rock, Win
    | Scissors, Paper -> Paper, Lose
    | Scissors, Scissors -> Scissors, Draw

let parse2 (line : string) : (Play * Goal) = 
    let ss = line.Split(" ")
    match (interpretElf ss.[0], interpretGoal ss.[1]) with 
    | Rock, Lose -> Scissors, Lose
    | Rock, Draw -> Rock, Draw
    | Rock, Win -> Paper, Win
    | Paper, Lose -> Rock, Lose
    | Paper, Draw -> Paper, Draw
    | Paper, Win -> Scissors, Win
    | Scissors, Lose -> Paper, Lose
    | Scissors, Draw -> Scissors, Draw
    | Scissors, Win -> Rock, Win

let scorePlay play = 
    match play with 
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let scoreGoal goal = 
    match goal with 
    | Lose -> 0
    | Draw -> 3
    | Win -> 6

let scoreRound (play, goal) = 
    scorePlay play + scoreGoal goal

let score parser lines = 
    lines |> Array.sumBy (parser >> scoreRound)

let run lines = 
    lines |> score parse1 |> printfn "%d" 
    lines |> score parse2 |> printfn "%d" 

"input.txt" |> File.ReadAllLines |> run 
