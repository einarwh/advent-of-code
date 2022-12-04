// Advent of Code 2020. Day 7.
// dotnet fsi aoc07.fsx

open System.IO
open System.Text.RegularExpressions

type Bag = Bag of string

type Bags = {
    bag : Bag
    count : int
}

type Rule = {
    bag : Bag 
    content : Bags list
}

let parseBags (s : string) : Bags =
    let m = Regex.Match(s, "^(\d+) ([a-z]+ [a-z]+)")
    let count = int m.Groups.[1].Value
    let bagName = m.Groups.[2].Value
    {
        bag = Bag bagName
        count = count
    }

let parseRule (s : string) : Rule =
    let m = Regex.Match(s, "^([a-z]+ [a-z]+) bags contain (.+)\.$")
    let bagName = m.Groups.[1].Value
    let contentStr = m.Groups.[2].Value
    let content =
        if contentStr = "no other bags" then
            []
        else 
            contentStr.Split(", ") |> Array.toList |> List.map parseBags
    {
        bag = Bag bagName
        content = content 
    }

let findDirectContainingBags (bag : Bag) (rules : Rule list) : Bag list =
    let containsBag content = content |> List.exists (fun { bag = b; count = _ } -> b = bag)
    rules
    |> List.choose (fun { bag = container; content = content } -> if containsBag content then Some container else None)

let rec findAllContainingBags (bag : Bag) (seen : Bag list) (rules : Rule list) : Bag list =
    findDirectContainingBags bag rules
    |> List.fold (fun acc b -> if List.contains b acc then acc else findAllContainingBags b (b::acc) rules) seen
    
let countContainingBags (bag : Bag) (rules : Rule list) : int =
    rules |> findAllContainingBags bag [] |> List.length
    
let rec countContainedBags (bag : Bag) (rules : Rule list) : int =
    rules
    |> List.pick (fun { bag = b; content = c } -> if b = bag then Some c else None)
    |> List.map (fun { bag = b; count = c } -> c * (1 + countContainedBags b rules))
    |> List.sum
        
let run lines =
    let rules = lines |> Array.toList |> List.map parseRule
    let goldBag = Bag "shiny gold"

    rules
    |> countContainingBags goldBag
    |> printfn "%d containing bags"

    rules |>
    countContainedBags goldBag
    |> printfn "%d contained bags"

"input" |> File.ReadAllLines |> run 
