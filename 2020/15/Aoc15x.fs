open System.Diagnostics

type History = History of Map<int, int> 

module History =

    let init = History Map.empty
        
    let add number spoken history =
        match history with 
        | History h -> History (h |> Map.add number spoken)        
        
    let lookup number history =
        match history with
        | History h -> h |> Map.tryFind number

let next (number, history) turn =
    let history' = history |> History.add number (turn - 1)
    match history |> History.lookup number with
    | None ->
        (0, history')
    | Some last ->
        (turn - 1 - last, history')

let initHistory startNumbers =
    let folder (turn, history) number =
        (turn + 1, history |> History.add number turn)
    let (_, history) = startNumbers |> List.fold folder (1, History.init)
    history

let getNumber endTurn startNumbers =
    let stopwatch = Stopwatch.StartNew()
    let lastSpoken = List.last startNumbers
    let history = startNumbers |> List.take (List.length startNumbers - 1) |> initHistory 
    let startTurn = 1 + List.length startNumbers
    let (number, _) = [startTurn .. endTurn] |> List.fold next (lastSpoken, history)
    (number, sprintf "%.2f seconds" stopwatch.Elapsed.TotalSeconds)

[<EntryPoint>]
let main _ =
    let numbers = [1;0;15;2;10;13]
    getNumber 2020 numbers |> printfn "%A"
    getNumber 30000000 numbers |> printfn "%A"
    0 
