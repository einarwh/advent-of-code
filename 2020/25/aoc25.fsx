// Advent of Code 2020. Day 25: Combo Breaker.
// dotnet fsi aoc25.fsx

open System
open System.IO

let transformStep (subjectNumber : int64) (value : int64) = 
    (value * subjectNumber) % 20201227L

let transform (subjectNumber : int64) (loopNumber : int64) =
    let rec loop (i : int64) (value : int64) = 
        if i < loopNumber then 
            let v = transformStep subjectNumber value 
            loop (i + 1L) v 
        else 
            value 
    loop 0 1 

let findLoopNumber (subjectNumber : int64) (publicKey : int64) = 
    let rec loop (i : int64) (value : int64) = 
        if value = publicKey then i 
        else 
            let v = transformStep subjectNumber value 
            loop (i + 1L) v 
    loop 0 1

let findEncryptionKey (cardPublicKey : int64) (doorPublicKey : int64) : int64 = 
    let cardLoopNumber = findLoopNumber 7 cardPublicKey
    transform doorPublicKey cardLoopNumber

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)

let run fileName = 
    let numbers = readLines fileName |> Array.map int64 
    let cardPublicKey = numbers[0]
    let doorPublicKey = numbers[1]
    findEncryptionKey cardPublicKey doorPublicKey |> printfn "%d"

run "input.txt"
