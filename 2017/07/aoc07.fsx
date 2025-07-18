// Advent of Code 2017. Day 07: Recursive Circus.
// dotnet fsi aoc07.fsx

open System
open System.IO

type Name = string

type Weight = int

type Disc = Name * Weight

type Tree = Leaf of Disc | Branch of Disc * Tree list 

exception UnbalancedTreeException of int

let split (sep : string) (s : string) =  
    s.Split([|sep|], StringSplitOptions.None) |> Seq.toList  

let readWeight (s : string) : Weight = 
    s.Substring(1, s.Length - 2) |> int

let readLeft (a : string) = 
    match split " " a with 
    | [n; w] ->
        Some (n, readWeight w)
    | x -> None
  
let readLine (line : string) = 
    if line.Contains(" -> ") then 
        match split " -> " line with 
        | [a; b] -> 
            readLeft a |> Option.map (fun (n, w) -> (n, (w, split ", " b))) 
        | _-> None
    else 
        readLeft line |> Option.map (fun (n, w) -> (n, (w, [])))

let rec buildTree root (map : Map<Name, Weight * Name list>) : Tree = 
    let (w, names) = Map.find root map
    let disc = root, w
    match names with 
    | [] -> Leaf disc
    | _ -> Branch (disc, names |> List.map (fun n -> buildTree n map)) 

let getRootDisc = function
    | Leaf d1 -> d1
    | Branch (d2, _) -> d2

let calculateCorrectWeight (weights : Weight list) (branches : Tree list) = 
    let discs = branches |> List.map getRootDisc
    let summaries = 
        List.zip discs weights 
        |> List.map (fun ((_, wx), weight) -> wx, weight)
        |> List.groupBy snd 
        |> List.map (fun (w, lst) -> (w, lst |> List.map fst))
        |> List.sortBy (fun (_, lst) -> List.length lst)
    match summaries with 
    | [(unbalanced, [w]); (balanced, _)] -> 
        w + balanced - unbalanced
    | _ -> failwith "?"

let rec checkTree root : Weight = 
    match root with
    | Leaf (n, w) -> w
    | Branch ((n, w), bs) ->
        let weights = bs |> List.map checkTree
        match weights with 
        | [] -> w
        | h :: t ->
            if List.exists (fun x -> x <> h) t then 
                let correct = calculateCorrectWeight weights bs 
                raise (UnbalancedTreeException correct)
            else 
                w + List.sum weights
 
let findCorrectWeight tree =
    try 
        checkTree tree 
    with
        UnbalancedTreeException weight -> weight

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let reverseMap (map : Map<Name, Weight*Name list>) = 
    let rec fn (acc : (Name*Name) list) (elements : (Name * (Weight*Name list)) list) = 
        match elements with 
        | [] -> acc |> Map.ofList 
        | (name, (_, names)) :: rest ->
            let rev = names |> List.map (fun n -> (n, name)) 
            fn (rev @ acc) rest
    let elements = Map.toList map 
    fn [] elements

let findRoot (map : Map<Name, Name>) = 
    let rec find name = 
        match map |> Map.tryFind name with 
        | Some n -> find n
        | None -> name
    let name = map |> Map.toList |> List.head |> snd
    find name 

let run fileName = 
    let lines = readLines fileName
    let map = lines |> List.choose readLine |> Map.ofList
    let revMap = reverseMap map
    let root = findRoot revMap
    root |> printfn "%s"
    let tree = buildTree root map
    tree |> findCorrectWeight |> printfn "%d"

run "input.txt"
