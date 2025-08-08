// Advent of Code 2020. Day 21: Allergen Assessment.
// dotnet fsi aoc21.fsx

open System.IO
open System.Text.RegularExpressions

let parseLine (line : string) = 
    let pattern = "^([a-z]+( [a-z]+)*) \(contains ([a-z]+(, [a-z]+)*)\)$"
    let m = Regex.Match(line, pattern)
    if m.Success then
        Some (m.Groups.[1].Value.Split(" ") |> Set.ofArray, m.Groups.[3].Value.Split(", ") |> Set.ofArray)
    else
        None

let getCandidateIngredients (foodList : (Set<string> * Set<string>) array) (allergen : string) = 
    foodList 
    |> Array.choose (fun (ingredients, allergens) -> if Set.contains allergen allergens then Some ingredients else None)
    |> Array.reduce Set.intersect

let countFoodsWithIngredient (ingredientsListList : Set<string> array) (ingredient : string) = 
    ingredientsListList
    |> Array.filter (Set.contains ingredient)
    |> Array.length

let pruneAllergen (allergen : string) (allergensWithIngredients : (string * Set<string>) list) : (string * Set<string>) list = 
    allergensWithIngredients
    |> List.filter (fun (a, xs) -> a <> allergen) 

let pruneIngredient (ingredient : string) (allergensWithIngredients : (string * Set<string>) list) : (string * Set<string>) list = 
    allergensWithIngredients
    |> List.map (fun (a, xs) -> (a, Set.remove ingredient xs))

let rec prune (allergensWithIngredients : (string * Set<string>) list) singles = 
    match singles with 
    | [] -> allergensWithIngredients
    | (allergen, ingredient) :: remaining -> 
        let allergensWithIngredients' = 
            allergensWithIngredients 
            |> pruneAllergen allergen
            |> pruneIngredient ingredient
        prune allergensWithIngredients' remaining

let rec identifyDangerousIngredients (result : (string * string) list) (allergensWithIngredients : (string * Set<string>) list) : (string * string) list = 
    let singles = 
        allergensWithIngredients
        |> List.filter (fun (a, xs) -> Set.count xs = 1)
        |> List.map (fun (a, xs) -> (a, xs |> Set.toList |> List.head))
    let ambivalents = 
        allergensWithIngredients
        |> List.filter (fun (a, xs) -> Set.count xs > 1)
    if List.isEmpty ambivalents then 
        result @ singles
    else 
        let identifiedAllergens = 
            singles |> List.map fst 
        let allergensWithIngredients' = 
            prune allergensWithIngredients singles
        identifyDangerousIngredients (result @ singles) allergensWithIngredients'

let canonical dangerousList = 
    dangerousList 
    |> List.sortBy (fun (a, i) -> a) 
    |> List.map snd 
    |> String.concat "," 

let run fileName = 
    let foodList = File.ReadAllLines fileName |> Array.choose parseLine
    let allergensListList = foodList |> Array.map snd
    let ingredientsListList = foodList |> Array.map fst
    let uniqueAllergens = allergensListList |> Set.unionMany
    let uniqueIngredients = ingredientsListList |> Set.unionMany 
    let allergensWithIngredients = 
        uniqueAllergens 
        |> Set.map (fun a -> (a, getCandidateIngredients foodList a))
        |> Set.toList 
    let possibleIngredients = 
        allergensWithIngredients
        |> List.map snd 
        |> Set.unionMany
    let innocentIngredients = 
        possibleIngredients
        |> Set.difference uniqueIngredients
    let innocentOccurrences = 
        innocentIngredients 
        |> Set.toList
        |> List.sumBy (countFoodsWithIngredient ingredientsListList)
    innocentOccurrences |> printfn "%A"
    let dangerous = identifyDangerousIngredients [] allergensWithIngredients
    dangerous |> canonical |> printfn "%s"
    
"input.txt" |> run
