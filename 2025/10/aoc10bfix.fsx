// Advent of Code 2025 - Day 10: Factory

#r "./z3/Microsoft.Z3.dll"

open Microsoft.Z3
open System
open System.IO

type Machine =
    { lights : bool array
      buttons : int array array
      joltage : int array }

let middleString (s : string) = s[1 .. s.Length - 2]

let middleArray (arr : 'a array) = arr[1 .. arr.Length - 2]
      
let parseLights (s : string) = 
    let booleans = middleString s |> Seq.toArray |> Array.map ((=) '#')
    booleans 
    
let parseNumbers (s : string) = 
    s.Split "," |> Array.map int

let parseButton numLights (s : string) = 
    middleString s |> parseNumbers

let parseJoltage (s : string) = 
    let numbers = middleString s |> parseNumbers
    numbers 

let parseMachine (s : string) = 
    let parts = s.Split " "
    let lights = parts |> Array.head |> parseLights
    let numLights = Array.length lights 
    let joltage = parts |> Array.last |> parseJoltage
    let buttons = middleArray parts |> Array.map (parseButton numLights)
    { lights = lights; buttons = buttons; joltage = joltage }

let solve (buttons: int array array) (joltages: int array) =
    use ctx = new Context()
    use opt = ctx.MkOptimize()

    let presses = 
        [| 0 .. Array.length buttons - 1|] 
        |> Array.map (fun i -> $"btn{i}" |> ctx.MkIntConst :> ArithExpr)

    presses |> Array.iter (fun p -> ctx.MkGe(p, ctx.MkInt 0) |> opt.Add)

    for i in 0 .. Array.length joltages - 1 do
        let affecting =
            [| 0 .. Array.length presses - 1 |]
            |> Array.filter (fun bix -> Array.contains i buttons[bix])
            |> Array.map (fun bix -> presses[bix])

        if Array.length affecting > 0 then
            ctx.MkEq(ctx.MkAdd affecting, ctx.MkInt joltages[i]) |> opt.Add

    ctx.MkAdd presses |> opt.MkMinimize |> ignore
    opt.Check() |> ignore
    presses |> Array.sumBy (fun p -> opt.Model.Eval(p, true) :?> IntNum |> _.Int64)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    let machines = lines |> List.map parseMachine
    machines |> List.sumBy (fun m -> solve m.buttons m.joltage) |> printfn "%d"

run "input.txt"