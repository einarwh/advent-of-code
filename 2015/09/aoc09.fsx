// Advent of Code 2015. Day 09: All in a Single Night.
// dotnet fsi aoc09.fsx

open System
open System.IO

let parse (s : string) : (string*string*int) option = 
    match s.Split(" ") with 
    | [|loc1; _; loc2; _; dist |] -> Some (loc1, loc2, int dist)
    | _ -> None

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let createMap distances = 
    let rec loop distances map = 
        match distances with 
        | [] -> map 
        | (loc1, loc2, dist) :: rest -> 
            map |> Map.add (loc1, loc2) dist |> Map.add (loc2, loc1) dist |> loop rest
    loop distances Map.empty

let getUniqueLocations distances = 
    let rec fn distances locs = 
        match distances with 
        | [] -> locs |> Set.toList 
        | (loc1, loc2, _) :: rest -> 
            locs |> Set.add loc1 |> Set.add loc2 |> fn rest 
    Set.empty |> fn distances

let getDistance (map : Map<string*string,int>) (path : string list)  = 
    path |> List.pairwise |> List.map (fun pair -> Map.find pair map) |> List.sum 

let run fileName = 
    let lines = readLines fileName
    let pairDistances = lines |> List.choose parse
    let locations = pairDistances |> getUniqueLocations
    let map = pairDistances |> createMap 
    let allDistances = locations |> permute |> List.map (getDistance map)
    allDistances |> List.min |> printfn "%d"
    allDistances |> List.max |> printfn "%d"

run "input.txt"
