// Advent of Code 2017. Day 07: Recursive Circus.
// dotnet fsi aoc07.fsx

open System
open System.IO


type Name = string

type Weight = int

type Disc = Name * Weight

type Tree = Leaf of Disc | Branch of Disc * Tree list 

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

let findCorrectWeight (weights : Weight list) (branches : Tree list) = 
    let discs = branches |> List.map getRootDisc
    let summaries = 
        List.zip discs weights 
        |> List.map (fun ((_, wx), weight) -> wx, weight)
    summaries 
    |> List.groupBy snd 
    |> List.map (fun (w, lst) -> (w, lst |> List.map fst))
    |> List.sortBy (fun (_, lst) -> List.length lst)

let rec checkTree root : Weight = 
  match root with
  | Leaf (n, w) -> w
  | Branch ((n, w), bs) ->
    let weights = bs |> List.map checkTree
    match weights with 
    | [] -> w
    | h :: t ->
      if List.exists (fun x -> x <> h) t then 
        let correct = findCorrectWeight weights bs 
        printfn "correct %A" correct
        let discs = bs |> List.map getRootDisc
        List.zip discs weights 
        |> List.map (fun ((n, wx), weight) -> n, wx, weight)
        |> sprintf "??? %A" 
        |> failwith 
      else 
        w + List.sum weights
 
let solve root lines = 
  let solidify = Seq.map readLine >> Seq.choose id >> Seq.toList
  let map = lines |> solidify |> Map.ofList
  let tree = buildTree root map
  checkTree tree
  ()

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
    printfn "%A" map
    let revMap = reverseMap map
    let root = findRoot revMap
    printfn "root %A" root
    let tree = buildTree root map
    printfn "%A" tree
    checkTree tree
    // lines |> printfn "%A"

run "sample.txt"
