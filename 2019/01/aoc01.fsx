// Advent of Code 2019. Day 1: The Tyranny of the Rocket Equation. 
// dotnet fsi aoc01.fsx

open System
open System.IO

let fuel mass = mass / 3 - 2

let zenoFuel mass = 
    let gen m = if m > 0 then Some (m, fuel m) else None
    mass |> fuel |> Seq.unfold gen |> Seq.sum 

let parse (s: string) =
    match Int32.TryParse(s) with 
    | (true, n) -> Some n
    | _ -> None

let calculate fuelFn = 
    Seq.choose parse >> Seq.map fuelFn >> Seq.sum

let run lines = 
    lines |> calculate fuel |> printfn "%d"
    lines |> calculate zenoFuel |> printfn "%d"

"input.txt" |> File.ReadAllLines |> run
