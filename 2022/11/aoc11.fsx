// Advent of Code 2022. 
// Day 10: Cathode-Ray Tube.
// dotnet fsi aoc09.fsx

open System
open System.IO
open System.Text.RegularExpressions

type BinaryOperand =
    | Number of int64 
    | Old

type Operation = int64 -> int64

type Monkey = 
    { Number : int 
      Items : int64 list 
      Operation : Operation
      Divisor : int64 
      TrueMonkey : int 
      FalseMonkey : int
      Inspections : int64 }

let double = fun (x : int64) -> x + x

let square = fun (x : int64) -> x * x

let multiplyBy n = fun (x : int64) -> x * n

let increaseBy n = fun (x : int64) -> x + n

let parseMonkeyNumberLine (s : string) : int = 
    let m = Regex.Match(s.Trim(), "^Monkey (\d+):$")
    if m.Success then
        m.Groups.[1].Value |> int
    else 
        failwith "invalid monkey number line"

let parseStartingItemsLine (s : string) : int64 list = 
    let m = Regex.Match(s.Trim(), "^Starting items: (.+)$")
    if m.Success then
        let str = m.Groups.[1].Value
        str.Split(", ") |> Array.toList |> List.map (int >> int64)
    else 
        failwith "invalid starting items line"

let toBinaryOperand a = 
    if a = "old" then Old 
    else a |> int64 |> Number 

let parseOperation (s : string) : Operation =
    match s.Split(" ") with 
    | [|"+"; "old"|] -> double
    | [|"+"; n|] -> n |> int64 |> increaseBy
    | [|"*"; "old"|] -> square
    | [|"*"; n|] -> n |> int64 |> multiplyBy
    | _ -> failwith "invalid binary operation"

let parseOperationLine (s : string) : Operation = 
    let m = Regex.Match(s.Trim(), "^Operation: new = old (.+)$")
    if m.Success then
        let str = m.Groups.[1].Value
        parseOperation str 
    else 
        failwith "invalid starting items line"

let parseTestLine (s : string) : int64 = 
    let m = Regex.Match(s.Trim(), "^Test: divisible by (\d+)$")
    if m.Success then
        m.Groups.[1].Value |> int64
    else 
        failwith "invalid starting items line"

let parseIfTrueLine (s : string) : int = 
    let m = Regex.Match(s.Trim(), "^If true: throw to monkey (\d+)$")
    if m.Success then
        m.Groups.[1].Value |> int
    else 
        failwith "invalid if true line"

let parseIfFalseLine (s : string) : int = 
    let m = Regex.Match(s.Trim(), "^If false: throw to monkey (\d+)$")
    if m.Success then
        m.Groups.[1].Value |> int
    else 
        failwith "invalid if false line"

let parseMonkey (s : string) = 
    match s.Split("\n") with 
    | [|a;b;c;d;e;f|] -> 
        { Number = parseMonkeyNumberLine a
          Items = parseStartingItemsLine b 
          Operation = parseOperationLine c 
          Divisor = parseTestLine d 
          TrueMonkey = parseIfTrueLine e
          FalseMonkey = parseIfFalseLine f
          Inspections = 0 }
    | _ -> failwith "unable to parse monkey"

let evaluateOperand (old : int64) operand = 
    match operand with 
    | Old -> old 
    | Number (n : int64) -> n

let applyOperation operation (item: int64) : int64 = 
    operation item 

let determineTargetMonkeyNumber (level : int64) (monkey : Monkey) = 
    let remainder = level % monkey.Divisor
    if remainder = 0 then 
        // printfn "    Current worry level is divisible by %d." monkey.Divisor
        monkey.TrueMonkey 
    else 
        // printfn "    Current worry level is not divisible by %d." monkey.Divisor
        monkey.FalseMonkey
    
let turn (relief : int64 -> int64) (monkey : Monkey) (monkeys : Monkey array) = 
    let rec fn (monkey : Monkey) (monkeys : Monkey array) : Monkey = 
        match monkey.Items with 
        | [] -> monkey
        | (item : int64) :: restItems -> 
            let updatedItem = item |> applyOperation monkey.Operation |> relief  
            let targetMonkeyNumber = determineTargetMonkeyNumber updatedItem monkey
            let targetMonkey = monkeys.[targetMonkeyNumber]
            let updatedTargetMonkey = { targetMonkey with Items = targetMonkey.Items @ [ updatedItem ] }
            Array.set monkeys targetMonkeyNumber updatedTargetMonkey
            let updatedMonkey = { monkey with Items = restItems; Inspections = monkey.Inspections + 1L }
            Array.set monkeys updatedMonkey.Number updatedMonkey
            fn updatedMonkey monkeys
    fn monkey monkeys |> ignore 

let printMonkeyStatus monkey =  
    let itemsStr = monkey.Items |> List.map string |> String.concat ", "
    printfn "Monkey %d: %s" monkey.Number itemsStr

let printInspectionStatus monkey =  
    printfn "Monkey %d: inspected items %d times." monkey.Number monkey.Inspections

let round (relief : int64 -> int64) (monkeys : Monkey array) = 
    let rec fn (monkeyNumber : int) (monkeys : Monkey array) = 
        if monkeyNumber < Array.length monkeys then 
            let monkey = monkeys.[monkeyNumber]
            turn relief monkey monkeys
            fn (monkeyNumber + 1) monkeys
            // printfn ""
    fn 0 monkeys

let rec runRounds n relief monkeys = 
    let rec loop (i : int) = 
        if i <= n then
            round relief monkeys 
            loop (i + 1)
    loop 1

let toMonkeyBusiness lst = 
    match lst with 
    | a :: b :: _ -> a * b 
    | _ -> failwith "failed to calculate monkey business"

let solve (rounds : int) (relief : int64 -> int64) (monkeys : Monkey array) =
    runRounds rounds relief monkeys
    monkeys 
    |> Array.toList 
    |> List.map (fun m -> m.Inspections)
    |> List.sortDescending 
    |> toMonkeyBusiness
    |> printfn "%d"

let part1 (monkeys : Monkey array) =
    let relief = fun item -> item / 3L 
    solve 20 relief monkeys

let part2 (monkeys : Monkey array) =
    let divisor = monkeys |> Array.map (fun m -> m.Divisor) |> Array.fold (*) 1L
    let relief = fun item -> item % divisor 
    solve 10000 relief monkeys

let run (monkeys : Monkey array) = 
    monkeys |> Array.copy |> part1 
    monkeys |> Array.copy |> part2

"input"
|> File.ReadAllText 
|> (fun s -> s.TrimEnd().Split("\n\n"))
|> Array.map parseMonkey
|> run
