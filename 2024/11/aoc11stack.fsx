// Advent of Code 2024. Day 11: Plutonian Pebbles.
// dotnet fsi aoc11.fsx

open System
open System.IO

let applyRules stone = 
    if stone = 0L then 
        [1L]
    else 
        let s = stone.ToString()
        if s.Length % 2 = 0 then 
            let s1 = s.Substring(0, s.Length / 2)
            let s2 = s.Substring(s.Length / 2)
            [ s1; s2 ] |> List.map int64
        else
            [stone * 2024L]

type Memo = Map<int64 * int, int64 list>

type Stack = (int64 * int) list 

let push item stack = item :: stack 

let pop stack = List.head stack 

let isEmpty stack = 0 = List.length stack

let blink (memo : Memo) (stone : int64, times : int) = 
    let rec loop memo (results : (int64 * int) list list) (stack : Stack) (stones : (int64 * int) list) =
        match stones with 
        | [] -> 
            // Start consuming the stack!
        | (stone, times) :: remaining -> 
            // Can I calculate this directly?
            match memo |> Map.tryFind (stone, times) with 
            | Some r -> 
                // Already cached! Add to result and keep going.
                loop memo (r :: results) stack remaining
            | None -> 
                // Not available yet.
                // if times = 1, just calculate it. Nothing to be saved.
                let r = stone |> applyRules
                if times = 1 then 
                    
                    let r = stone |> applyRules 
                // Push current onto stack. 
                // Push all next level stones onto stones.
                let nextLevel = stone |> applyRules |> List.map (fun s -> (s, times - 1))
                loop memo 

        // Check stack first? Or when? 
        // Maybe the thing should just be first item on stack? 
        // Yes, probably.
        if isEmpty stack then 
            printfn "Empty. Now what?"
            failwith "stack underflow"
        else 
            let (stone, times) = stack |> pop 
            match memo |> Map.tryFind (stone, times) with 
            | Some result -> (memo, result)
            | None -> 
                // Can I calculate directly?
                // Are all subcalculations available? 
                // Well here we go. Keep this problem for later.
                let stk = stack |> push (stone, times)
                // When to update memo?









let run fileName = 
    let text = File.ReadAllText fileName 
    let stones = text.Trim().Split(" ") |> Array.toList |> List.map int64 
    stones |> blinking 5 |> List.length |> printfn "%d"
    // stones |> blinking 75 |> List.length |> printfn "%d"

run "sample2"
