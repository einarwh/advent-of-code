// Advent of Code 2015. Day 13
// dotnet fsi aoc13.fsx

open System
open System.IO
open System.Text.RegularExpressions

let parse (s : string) : (string*string*int) option =
    let m = Regex.Match(s, "^([A-Za-z]+) would (lose|gain) (\d+) happiness units by sitting next to ([A-Za-z]+)\.$")
    if m.Success then
        let name1 = m.Groups.[1].Value
        let effect = if m.Groups.[2].Value = "gain" then 1 else -1
        let amount = int m.Groups.[3].Value
        let name2 = m.Groups.[4].Value
        Some (name1, name2, effect * amount)
    else
        None

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

let happiness (map : Map<string,Map<string,int>>) (seating : string list) : int = 
    let arr = seating |> List.toArray
    let lastIndex = arr.Length - 1
    let leftOf (i : int) = if i = 0 then lastIndex else i - 1
    let rightOf (i : int) = if i = lastIndex then 0 else i + 1
    let score (i : int) = 
        let guest = arr[i]
        let leftNeighbor = arr[leftOf i]
        let rightNeighbor = arr[rightOf i]
        match Map.tryFind guest map with 
        | None -> 0 
        | Some guestMap -> 
            let leftScore = Map.tryFind leftNeighbor guestMap |> Option.defaultValue 0
            let rightScore = Map.tryFind rightNeighbor guestMap |> Option.defaultValue 0 
            leftScore + rightScore
    [ 0 .. arr.Length - 1 ] |> List.map score |> List.sum

let run fileName = 
    let lines = readLines fileName
    let map = 
        lines 
        |> List.choose parse
        |> List.groupBy (fun (n, _, _) -> n) 
        |> List.map (fun (n, lst) -> (n, lst |> List.map (fun (_, n, e) -> (n, e)) |> Map.ofList))
        |> Map.ofList
    let guests = map |> Map.keys |> Seq.toList
    guests |> permute |> List.map (happiness map) |> List.max |> printfn "%A"
    "Me" :: guests |> permute |> List.map (happiness map) |> List.max |> printfn "%A"

run "input.txt"
