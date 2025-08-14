// Advent of Code 2015. Day 20: Infinite Elves and Infinite Houses.
// dotnet fsi aoc20.fsx

open System
open System.IO

let primes =
    let rec nextPrime n p primes =
        if primes |> Map.containsKey n then
            nextPrime (n + p) p primes
        else
            primes.Add(n, p)

    let rec prime n primes =
        seq {
            if primes |> Map.containsKey n then
                let p = primes.Item n
                yield! prime (n + 1) (nextPrime (n + p) p (primes.Remove n))
            else
                yield n
                yield! prime (n + 1) (primes.Add(n * n, n))
        }

    prime 2 Map.empty

let primeFactors (n : int) = 
    printfn "n %d" n
    // let maxPrime = n / 2
    // printfn "maxPrime %d" maxPrime
    let candidates = primes |> Seq.takeWhile (fun p -> p <= int (Math.Sqrt n)) |> Seq.toList
    printfn "candidates %A" candidates
    let rec loop (acc : int list) (x : int) (ps : int list) = 
        match ps with 
        | [] -> 
            printfn "n=%d, x=%d, acc.Length=%d" n x acc.Length
            (n :: acc) |> List.rev
        | p :: ps' -> 
            if x % p = 0 then 
                loop (p :: acc) (x / p) ps' 
            else 
                loop acc x ps'
    loop [] n candidates 

let presents (houseNumber : int) =  
    let elves = houseNumber |> primeFactors
    printfn "elves for %d : %A" houseNumber elves
    houseNumber:: elves |> List.sumBy (fun e -> e * 10)

let solve limit = 
    let rec loop houseNumber = 
        if presents houseNumber >= limit then houseNumber else loop (houseNumber + 1)
    loop (limit / 80)

let factors number = 
    seq {
        for divisor in 1 .. (float >> sqrt >> int) number do
        if number % divisor = 0 then
            yield (number, divisor)
            yield (number, number / divisor) }

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    [1 .. 20] |> List.map presents |> List.iter (printfn "%d")

run "input.txt"
