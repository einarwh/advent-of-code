// Advent of Code 2015. Day 15: Science for Hungry People.
// dotnet fsi aoc15.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Ingredient = {
    name : string 
    capacity : int64 
    durability : int64 
    flavor : int64 
    texture : int64 
    calories : int64 
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

let calculateScore (caloriesConstraint : bool) (teaspoons : int list) (ingredients : Ingredient list) =
    let zipped = List.zip teaspoons ingredients
    let capacity = zipped |> List.map (fun (ts, ingr) -> int64 ts * ingr.capacity) |> List.sum |> max 0L
    let durability = zipped |> List.map (fun (ts, ingr) -> int64 ts * ingr.durability) |> List.sum |> max 0L
    let flavor = zipped |> List.map (fun (ts, ingr) -> int64 ts * ingr.flavor) |> List.sum |> max 0L
    let texture = zipped |> List.map (fun (ts, ingr) -> int64 ts * ingr.texture) |> List.sum |> max 0L
    let calories = zipped |> List.map (fun (ts, ingr) -> int64 ts * ingr.calories) |> List.sum |> max 0L
    if not caloriesConstraint || calories = 500 then 
        capacity * durability * flavor * texture 
    else 
        0L

let solve (caloriesConstraint : bool) (ingredients : Ingredient list) = 
    let rec loop (teaspoonsSpent : int list) (teaspoonsLeft : int) (ingredientsLeft : int) = 
        match ingredientsLeft with 
        | 0 -> 
            let teaspoons = teaspoonsSpent |> List.rev 
            let score = calculateScore caloriesConstraint teaspoons ingredients
            [score]
        | 1 -> 
            loop (teaspoonsLeft :: teaspoonsSpent) 0 0
        | _ -> 
            [0 .. teaspoonsLeft] |> List.collect (fun ts -> loop (ts :: teaspoonsSpent) (teaspoonsLeft - ts) (ingredientsLeft - 1))
    loop [] 100 (ingredients |> List.length)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let ingredients = lines |> List.choose tryParse
    ingredients |> solve false |> List.max |> printfn "%d"
    ingredients |> solve true |> List.max |> printfn "%d"

run "input.txt"
