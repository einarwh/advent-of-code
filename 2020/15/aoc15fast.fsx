open System.Diagnostics

let solve times startNumbers =
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

let run numbers =
    solve 2020 numbers |> printfn "%A"
    solve 30000000 numbers |> printfn "%A"
    0

[|1;0;15;2;10;13|] |> run 