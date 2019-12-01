<Query Kind="FSharpProgram" />

let fuel m =  m / 3 - 2

let zenoFuel mass = 
    let gen m = if m > 0 then Some (m, fuel m) else None
    mass |> fuel |> Seq.unfold gen |> Seq.sum 

let read path = 
    let parseMass s =
        match Int32.TryParse(s) with 
        | (true, n) -> Some n
        | _ -> None
    File.ReadLines(path) |> Seq.choose parseMass

let run = read >> Seq.map zenoFuel >> Seq.sum    
    
let path = "C:/einarwh/github/einarwh/advent-of-code/2019/01/input"
let total = run path

total |> printfn "%d"