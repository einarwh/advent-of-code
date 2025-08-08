// Advent of Code 2021. Day 10. 
// dotnet fsi aoc10.fsx

open System.IO

let toErrorPoints (ch : char) : int = 
    match ch with 
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "unknown char"

let toCompletionPoints (ch : char) : int64 = 
    match ch with 
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> failwith "unknown char"

let toCompletionScore (completion : char list) : int64 = 
    completion |> List.fold (fun total ch -> 5L * total + toCompletionPoints ch) 0L

let rec evaluate (stack : char list) (index : int) (input : string) : Result<char list, char> = 
    if index < input.Length then 
        let ch = input.[index]
        match ch with 
        | '(' 
        | '['
        | '{' 
        | '<' -> 
            evaluate (ch::stack) (index+1) input
        | _ -> 
            match stack with 
            | [] -> 
                failwith "underflow"
            | h::t -> 
                match h, ch with 
                | '(', ')'
                | '[', ']'
                | '{', '}'
                | '<', '>' -> 
                    evaluate t (index+1) input
                | _ -> 
                    Error ch
    else 
        Ok stack

let rec complete (result : char list) (stack : char list) : char list = 
    match stack with 
    | [] -> result |> List.rev 
    | h :: t -> 
        let closing = 
            match h with 
            | '(' -> ')'
            | '[' -> ']'
            | '{' -> '}'
            | '<' -> '>'
            | _ -> failwith "unknown char"
        complete (closing :: result) t

let run fileName = 
    let results = 
        fileName 
        |> File.ReadAllLines 
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map (evaluate [] 0)
    let errors = results |> Array.choose (fun r -> match r with | Ok _ -> None | Error ch -> Some ch) 
    errors |> Array.map toErrorPoints |> Array.sum |> printfn "Error points: %d"
    let incompletes = results |> Array.choose Result.toOption
    let completes = incompletes |> Array.map (complete []) |> Array.toList
    let scores = completes |> List.map toCompletionScore |> List.sort
    scores |> List.item ((List.length scores) / 2) |> printfn "Middle completion score: %d"

run "input.txt"
