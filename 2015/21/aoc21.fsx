// Advent of Code 2015. Day 21
// dotnet fsi aoc21.fsx

open System
open System.IO

type Outcome = Win of int | Loss of int 

type Stats = {
    hitPoints : int 
    damage : int 
    armor : int 
}

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

let getPlayerStats (specs : Spec list) = 
    { hitPoints = 100 
      damage = specs |> List.sumBy (fun s -> s.damage) 
      armor = specs |> List.sumBy (fun s -> s.armor) }

let fight (boss : Stats) (items : Item list) : Outcome = 
    let specs = items |> List.map snd 
    let player = getPlayerStats specs
    let cost = specs |> List.sumBy (fun s -> s.cost)
    let rec round playerHp bossHp = 
        let bossHp' = bossHp - max 1 (player.damage - boss.armor)
        if bossHp' > 0 then 
            let playerHp' = playerHp - max 1 (boss.damage - player.armor)
            if playerHp' > 0 then 
                round playerHp' bossHp' 
            else 
                Loss cost 
        else 
            Win cost
    round player.hitPoints boss.hitPoints

let fightToLose (boss : Stats) (items : Item list) = 
    let specs = items |> List.map snd 
    let player = getPlayerStats specs
    let rec round playerHp bossHp = 
        let bossHp' = bossHp - max 1 (player.damage - boss.armor)
        if bossHp' > 0 then 
            let playerHp' = playerHp - max 1 (boss.damage - player.armor)
            if playerHp' > 0 then 
                round playerHp' bossHp' 
            else 
                None 
        else 
            specs |> List.sumBy (fun s -> s.cost) |> Some
    round player.hitPoints boss.hitPoints

let findCheapestWin (boss : Stats) (combos : Item list list) = 
    let fightToWin (boss : Stats) (items : Item list) = 
        match fight boss items with 
        | Win cost -> Some cost 
        | _ -> None 
    combos |> List.choose (fightToWin boss) |> List.sort |> List.head

let findMostExpensiveLoss (boss : Stats) (combos : Item list list) = 
    let fightToLose (boss : Stats) (items : Item list) = 
        match fight boss items with 
        | Loss cost -> Some cost 
        | _ -> None 
    combos |> List.choose (fightToLose boss) |> List.sortDescending |> List.head 

let parse (s : string) = 
    (s.Split ": ")[1] |> int

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let numbers = readLines fileName |> Array.map parse 
    let boss = {
        hitPoints = numbers[0]
        damage = numbers[1]
        armor = numbers[2]
    }
    let combos = makeCombinations weaponPermutations armorPermutations ringPermutations
    combos |> findCheapestWin boss |> printfn "%d"
    combos |> findMostExpensiveLoss boss |> printfn "%d"

run "input.txt"
