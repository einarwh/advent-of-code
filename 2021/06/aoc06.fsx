// Advent of Code 2021. Day 6.
// dotnet fsi aoc06.fsx

open System.IO
open System.Linq

let simulate times map = 
    let rec evolve times zero one two three four five six seven eight = 
        if times > 0 then 
            evolve (times - 1) one two three four five six (zero + seven) eight zero
        else 
            [zero; one; two; three; four; five; six; seven; eight] |> List.sum
    let count key = map |> Map.tryFind key |> Option.defaultValue 0 |> int64
    let zero = count 0 
    let one = count 1  
    let two = count 2  
    let three = count 3  
    let four = count 4  
    let five = count 5  
    let six = count 6  
    let seven = count 7  
    let eight = count 8  
    evolve times zero one two three four five six seven eight

let run file times = 
    file
    |> File.ReadAllText 
    |> fun text -> text.Trim().Split(',')
    |> Seq.countBy int
    |> Map.ofSeq
    |> simulate times
    |> printfn "%d"    

run "input" 80
run "input" 256
