open System.Diagnostics

let run times startNumbers =
    let stopwatch = Stopwatch.StartNew()
    let size = max times (1 + Array.max startNumbers)
    let history = Array.create size 0
    let noStartNumbers = Array.length startNumbers
    for i in [1 .. noStartNumbers] do
        let startNumber = startNumbers.[i - 1]
        history.[startNumber] <- i
    let mutable lastSpoken = Array.last startNumbers
    for turn in (noStartNumbers + 1) .. Array.length history do
        let prev = history.[lastSpoken]
        history.[lastSpoken] <- (turn - 1)
        if prev = 0 then
            lastSpoken <- 0
        else
            lastSpoken <- (turn - 1 - prev)
    (lastSpoken, sprintf "%.2f ms" stopwatch.Elapsed.TotalMilliseconds)

[<EntryPoint>]
let main argv =
    let myStartNumbers = [|1;0;15;2;10;13|]
    run 2020 myStartNumbers |> printfn "%A"
    run 30000000 myStartNumbers |> printfn "%A"
    0