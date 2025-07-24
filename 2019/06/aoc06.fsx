// Advent of Code 2019. Day 06: Universal Orbit Map.
// dotnet fsi aoc06.fsx

open System
open System.IO

type SpaceObject = string

type Tree = Leaf of SpaceObject | Branch of SpaceObject * Tree list 

let rec buildTree root (map : Map<SpaceObject, SpaceObject list>) : Tree = 
    match Map.tryFind root map with 
    | Some objects -> Branch (root, objects |> List.map (fun n -> buildTree n map)) 
    | None -> Leaf root 

let findRoot (map : Map<SpaceObject, SpaceObject>) = 
    let rec find (name : SpaceObject) = 
        match map |> Map.tryFind name with 
        | Some n -> find n
        | None -> name
    let name = map |> Map.toList |> List.head |> snd
    find name 

let reverseMap (map : Map<SpaceObject, SpaceObject list>) = 
    let rec fn (acc : (SpaceObject * SpaceObject) list) (elements : (SpaceObject * (SpaceObject list)) list) = 
        match elements with 
        | [] -> acc |> Map.ofList 
        | (name, names) :: rest ->
            let rev = names |> List.map (fun n -> (n, name)) 
            fn (rev @ acc) rest
    let elements = Map.toList map 
    fn [] elements

let countOrbits (tree : Tree) : int = 
    let rec fn (depth : int) (tree : Tree) = 
        match tree with 
        | Leaf _ -> depth 
        | Branch (_, subtrees) -> 
            depth + (subtrees |> List.sumBy (fun t -> fn (depth + 1) t))
    fn 0 tree

let getPath (map : Map<SpaceObject, SpaceObject>) (obj : SpaceObject) = 
    let rec fn acc obj = 
        match Map.tryFind obj map with 
        | Some parent -> 
            fn (parent :: acc) parent 
        | None -> 
            List.rev acc 
    fn [] obj

let tryParse (line : string) : (SpaceObject * SpaceObject) option = 
    match line.Split ")" with 
    | [|a; b|] -> Some (a, b)
        | _-> None

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let findTransfers revMap = 
    let you = "YOU" |> getPath revMap |> Set.ofList
    let san = "SAN" |> getPath revMap |> Set.ofList
    let justYou = Set.difference you san |> Set.count
    let justSan = Set.difference san you |> Set.count
    justYou + justSan 

let run fileName = 
    let lines = readLines fileName
    let directOrbits = lines |> List.choose tryParse 
    let grouped = directOrbits |> List.groupBy fst |> List.map (fun (o, lst) -> (o, lst |> List.map snd))
    let map = grouped |> Map.ofList 
    let revMap = reverseMap map
    let root = findRoot revMap 
    let tree = buildTree root map 
    tree |> countOrbits |> printfn "%d"
    revMap |> findTransfers |> printfn "%d"

run "input.txt"
