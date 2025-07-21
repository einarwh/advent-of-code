// Advent of Code 2018. Day 07
// dotnet fsi aoc07.fsx

open System
open System.IO

let parse (s : string) = 
    let a = s.Split(" ")
    a[7], a[1]

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let determineOrder (lst : List<string*Set<string>>) = 
    let rec loop (steps : string list) (lst : List<string*Set<string>>) = 
        let candidates = lst |> List.filter (fun (_, reqs) -> Set.isEmpty reqs) |> List.sort
        match candidates with 
        | [] -> steps |> List.rev 
        | (chosenStep, _) :: _ -> 
            let lst' = 
                lst |> List.map (fun (s, reqs) -> (s, reqs |> Set.remove chosenStep))
                    |> List.filter (fun (s, _) -> s <> chosenStep)
            loop (chosenStep :: steps) lst'
    loop [] lst

// let completeSteps (lst : List<string*Set<string>>) = 
//     let rec loop (time : int) (available : int) (working : int list) (steps : string list) (lst : List<string*Set<string>>)
//         let candidates = lst |> List.filter (fun (_, reqs) -> Set.isEmpty reqs) |> List.sort

let run fileName = 
    let lines = readLines fileName
    let lst = 
        lines 
        |> List.map parse 
        |> List.groupBy fst 
        |> List.map (fun (step, rs) -> (step, rs |> List.map snd |> Set.ofList))
    let allStepsInReqs = lst |> List.fold (fun set (_, rs) -> Set.union rs set) Set.empty |> Set.toList
    let allStepsWithReqs = lst |> List.map fst 
    let stepsWithoutReqs = Set.difference (Set.ofList allStepsInReqs) (Set.ofList allStepsWithReqs) |> Set.toList
    let lst' = (stepsWithoutReqs |> List.map (fun s -> (s, Set.empty))) @ lst
    lst' |> determineOrder |> String.concat "" |> printfn "%s"

run "input.txt"
