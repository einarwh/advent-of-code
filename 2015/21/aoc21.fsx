// Advent of Code 2015. Day 21
// dotnet fsi aoc21.fsx

open System
open System.IO

type Spec = {
    cost : int 
    damage : int 
    armor : int 
}

type Item = string * Spec

let weapons = [
    "Dagger",     { cost =  8; damage = 4; armor = 0 }
    "Shortsword", { cost = 10; damage = 5; armor = 0 }
    "Warhammer",  { cost = 25; damage = 6; armor = 0 }
    "Longsword",  { cost = 40; damage = 7; armor = 0 }
    "Greataxe",   { cost = 74; damage = 8; armor = 0 }
]

let armors = [
    "Leather",    { cost =  13; damage = 0; armor = 1 }
    "Chainmail",  { cost =  31; damage = 0; armor = 2 }
    "Splintmail", { cost =  53; damage = 0; armor = 3 }
    "Bandedmail", { cost =  75; damage = 0; armor = 4 }
    "Platemail",  { cost = 102; damage = 0; armor = 5 }    
]

let rings = [
    "Damage +1",  { cost =  25; damage = 1; armor = 0 }
    "Damage +2",  { cost =  50; damage = 2; armor = 0 }
    "Damage +3",  { cost = 100; damage = 3; armor = 0 }
    "Defense +1", { cost =  20; damage = 0; armor = 1 }
    "Defense +2", { cost =  40; damage = 0; armor = 2 }
    "Defense +3", { cost =  80; damage = 0; armor = 3 }    
]

let selectOne items = 
    items |> List.map (fun it -> [it])

let selectTwo items =
    let rec loop items = 
        match items with 
        | [] -> failwith "empty"
        | [_] -> failwith "empty"
        | [a; b] -> [[a; b]]
        | it :: rest -> 
            let lst1 = selectOne rest |> List.map (fun lst -> it :: lst)
            let lst2 = loop rest 
            lst1 @ lst2 
    loop items 

let weaponPermutations : Item list list = 
    selectOne weapons 

let armorPermutations = 
    [] :: selectOne armors 

let ringPermutations = 
    [] :: selectOne rings @ selectTwo rings 

let makeCombinations (weaponPerms : Item list list) (armorPerms : Item list list) (ringPerms : Item list list) =
    let combos : Item list list = 
        weaponPerms 
        |> List.collect (fun w -> armorPerms |> List.map (fun a -> w @ a))
        |> List.collect (fun wa -> ringPerms |> List.map (fun r -> wa @ r))
    combos 

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    // lines |> printfn "%A"
    // ringPermutations |> List.iter (printfn "%A")
    let combos = makeCombinations weaponPermutations armorPermutations ringPermutations

run "input.txt"
