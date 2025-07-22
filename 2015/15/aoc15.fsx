// Advent of Code 2015. Day 15
// dotnet fsi aoc15.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Ingredient = {
    name : string 
    capacity : int 
    durability : int 
    flavor : int 
    texture : int 
    calories : int 
}

let tryParse (s : string) : Ingredient option = 
    let m = Regex.Match(s, "^([a-zA-Z]+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$")
    if m.Success then
        let name = m.Groups.[1].Value 
        let capacity = m.Groups.[2].Value |> int 
        let durability = m.Groups.[3].Value |> int 
        let flavor = m.Groups.[4].Value |> int 
        let texture = m.Groups.[5].Value |> int 
        let calories = m.Groups.[6].Value |> int 
        Some { name = name; capacity = capacity; durability = durability; flavor = flavor; texture = texture; calories = calories }
    else
        None

let solve (ingredients : Ingredient list) = 
    let rec loop (teaspoonsSpent : int list) (teaspoonsLeft : int) (ingredientsLeft : Ingredient list) : int list = 
        match ingredientsLeft with 
        | [] -> 
            teaspoonsSpent |> List.rev 
        | [last] -> 
            loop (teaspoonsLeft :: teaspoonsSpent) 0 []
        | ingr :: rest -> 
            [0 .. teaspoonsLeft] |> List.collect (fun ts -> loop (ts :: teaspoonsSpent) (teaspoonsLeft - ts) rest)
    loop [] 100 ingredients

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    lines |> List.choose tryParse |> printfn "%A"

run "input.txt"
