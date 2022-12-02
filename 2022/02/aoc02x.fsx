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

let parse1 (line : string) : (Play * Play) = 
    let ss = line.Split(" ")
    (interpretElf ss.[0], interpretPlay ss.[1])

let parse2 (line : string) : (Play * Goal) = 
    let ss = line.Split(" ")
    (interpretElf ss.[0], interpretGoal ss.[1])

let solve1 (elf, play) : (Play * Goal) = 
    match (elf, play) with 
    | Rock, Rock -> Rock, Draw
    | Rock, Paper -> Paper, Win
    | Rock, Scissors -> Scissors, Lose
    | Paper, Rock -> Rock, Lose
    | Paper, Paper -> Paper, Draw
    | Paper, Scissors -> Scissors, Win
    | Scissors, Rock -> Rock, Win
    | Scissors, Paper -> Paper, Lose
    | Scissors, Scissors -> Scissors, Draw

let solve2 (elf, goal) : (Play * Goal) = 
    match (elf, goal) with 
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

let score parser solver lines = 
    lines |> Array.map (parser >> solver >> scoreRound) |> Array.sum

let run lines = 
    lines |> score parse1 solve1 |> printfn "%A" 
    lines |> score parse2 solve2 |> printfn "%A" 

"input" |> File.ReadAllLines |> run 
