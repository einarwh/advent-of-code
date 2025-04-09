// Advent of Code 2019. Day 2: 1202 Program Alarm.
// dotnet fsi aoc02.fsx

open System.IO

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let execute (index : int) (program : int64 array) = 
    let rec loop ix = 
        match program[ix] with 
        | 99L -> program[0]
        | 1L -> 
            program[(int)program[ix+3]] <- program[(int)program[ix+1]] + program[(int)program[ix+2]]
            loop (ix+4)
        | 2L -> 
            program[(int)program[ix+3]] <- program[(int)program[ix+1]] * program[(int)program[ix+2]]
            loop (ix+4)
        | _ -> failwith <| sprintf "Unknown opcode %d at index %d" program[ix] ix
    loop index 

let runWith noun verb program = 
    let clone = program |> Seq.toArray 
    clone[1] <- noun
    clone[2] <- verb
    execute 0 clone 

let findTargetOutput targetOutput program = 
    let rec loop (noun : int) (verb : int) = 
        if noun < 100 then 
            let result = runWith noun verb program 
            if result = targetOutput then 
                noun * 100 + verb
            else 
                loop (noun + 1) verb 
        else if verb < 100 then 
            loop 0 (verb + 1)
        else 
            failwith "not found"
    loop 0 0 

let solve1 (program : int64 array) = 
    let clone = program |> Seq.toArray 
    clone[1] <- 12
    clone[2] <- 2
    execute 0 clone |> printfn "%d"

let solve2 (program : int64 array) = 
    let result = findTargetOutput 19690720L program
    result |> printfn "%A"

let solve fileName = 
    let program = File.ReadAllText fileName |> trim |> split "," |> Array.map int64
    solve1 program 
    solve2 program

solve "input"