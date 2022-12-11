// Advent of Code 2022. 
// Day 10: Cathode-Ray Tube.
// dotnet fsi aoc09.fsx

open System.IO
open System.Text.RegularExpressions

type BinaryOperand =
    | Number of int64 
    | Old

type BinaryOperation = 
    | Add of (BinaryOperand * BinaryOperand)
    | Multiply of (BinaryOperand * BinaryOperand)

type Monkey = 
    { Number : int 
      Items : int64 list 
      Operation : BinaryOperation
      Divisor : int64 
      TrueMonkey : int 
      FalseMonkey : int
      Inspections : int }

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
        str.Split(", ") |> Array.toList |> List.map int64
    else 
        failwith "invalid starting items line"

let toBinaryOperand a = 
    if a = "old" then Old 
    else int64 a |> Number 

let parseBinaryOperation (s : string) : BinaryOperation =
    match s.Split(" ") with 
    | [|a; b; c|] -> 
        match b with 
        | "+" -> 
            Add (toBinaryOperand a, toBinaryOperand c)
        | "*" -> 
            Multiply (toBinaryOperand a, toBinaryOperand c)
        | _ -> 
            failwith "invalid binary operation"
    | _ -> failwith "invalid binary operation"

let parseOperationLine (s : string) : BinaryOperation = 
    let m = Regex.Match(s.Trim(), "^Operation: new = (.+)$")
    if m.Success then
        let str = m.Groups.[1].Value
        parseBinaryOperation str 
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

let throwToMonkey (item : int64) (targetMonkeyNumber : int) (monkeys : Monkey array) =
    // printfn "    Item with worry level %d is thrown to monkey %d." item targetMonkeyNumber
    let targetMonkey = monkeys.[targetMonkeyNumber]
    let updatedTargetMonkey = { targetMonkey with Items = targetMonkey.Items @ [item] }
    Array.set monkeys targetMonkeyNumber updatedTargetMonkey

let evaluateOperand old operand = 
    match operand with 
    | Old -> old 
    | Number n -> n

let applyOperation operation item = 
    match operation with 
    | Add (a, b) -> 
        let op1 = (evaluateOperand item a)
        let op2 = (evaluateOperand item b)
        let result = (evaluateOperand item a) + (evaluateOperand item b)
        // if b = Old then 
        //     printfn "    Worry level increases with itself to %d." result
        // else 
        //     printfn "    Worry level increates with %d to %d" op2 result
        result
    | Multiply (a, b) -> 
        let op1 = (evaluateOperand item a)
        let op2 = (evaluateOperand item b)
        let result = (evaluateOperand item a) * (evaluateOperand item b)
        // if b = Old then 
        //     printfn "    Worry level is multiplied by itself to %d." result
        // else 
        //     printfn "    Worry level is multiplied by %d to %d" op2 result
        result

let reduceWorry item = 
    let result = item / 3L
    // printfn "    Monkey gets bored with item. Worry level is divided by 3 to %d." result
    result 

let determineTargetMonkeyNumber (level : int64) (monkey : Monkey) = 
    if level % monkey.Divisor = 0 then 
        // printfn "    Current worry level is divisible by %d." monkey.Divisor
        monkey.TrueMonkey 
    else 
        // printfn "    Current worry level is not divisible by %d." monkey.Divisor
        monkey.FalseMonkey
    
let turn (monkey : Monkey) (monkeys : Monkey array) = 
    let rec fn (monkey : Monkey) (monkeys : Monkey array) : Monkey = 
        match monkey.Items with 
        | [] -> monkey
        | (item: int64) :: restItems -> 
            // printfn "  Monkey inspects an item with worry level %d" item
            let updatedItem = item |> applyOperation monkey.Operation |> reduceWorry 
            let targetMonkeyNumber = determineTargetMonkeyNumber updatedItem monkey
            let targetMonkey = monkeys.[targetMonkeyNumber]
            let updatedTargetMonkey = { targetMonkey with Items = targetMonkey.Items @ [ updatedItem ] }
            Array.set monkeys targetMonkeyNumber updatedTargetMonkey
            let updatedMonkey = { monkey with Items = restItems; Inspections = monkey.Inspections + 1 }
            Array.set monkeys updatedMonkey.Number updatedMonkey
            fn updatedMonkey monkeys
    fn monkey monkeys |> ignore 

let round (roundNo : int) (monkeys : Monkey array) = 
    let rec fn (monkeyNumber : int) (monkeys : Monkey array) = 
        if monkeyNumber < Array.length monkeys then 
            // printfn "Monkey %d:" monkeyNumber
            let monkey = monkeys.[monkeyNumber]
            turn monkey monkeys
            fn (monkeyNumber + 1) monkeys
            // printfn ""
    fn 0 monkeys

let printMonkeyStatus monkey =  
    let itemsStr = monkey.Items |> List.map string |> String.concat ", "
    printfn "Monkey %d: %s" monkey.Number itemsStr

let rec runRounds i n monkeys = 
    if i <= n then 
        round i monkeys 
        runRounds (i + 1) n monkeys    

let toMonkeyBusiness lst = 
    match lst with 
    | a :: b :: _ -> a * b 
    | _ -> failwith "failed to calculate monkey business"

let run (monkeys : Monkey array) = 
    runRounds 1 20 monkeys
    monkeys 
    |> Array.toList 
    |> List.map (fun m -> m.Inspections)
    |> List.sortDescending 
    |> toMonkeyBusiness
    |> printfn "%d"

"input"
|> File.ReadAllText 
|> (fun s -> s.TrimEnd().Split("\n\n"))
|> Array.map parseMonkey
|> run
