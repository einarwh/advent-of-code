<Query Kind="FSharpProgram" />

let fuel mass = mass / 3 - 2

let read path = 
    let parseMass s =
        match Int32.TryParse(s) with 
        | (true, n) -> Some n
        | _ -> None
    File.ReadLines(path) |> Seq.choose parseMass
    
let run = read >> Seq.map fuel >> Seq.sum
    
let path = "C:/einarwh/github/einarwh/advent-of-code/2019/01/input"
let total = run path

total |> printfn "total %d"