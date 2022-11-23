type Spoken =
    | Once of int
    | More of int * int 

let remember number turn history =
    match history |> Map.tryFind number with
    | None -> history |> Map.add number (Once turn)
    | Some before ->
        let prev = match before with | Once first -> first | More (last, _) -> last
        history |> Map.add number (More (turn, prev))

let next (number, history) turn =
    let next = 
        match history |> Map.tryFind number with
        | None -> 0
        | Some spoken ->
            match spoken with
            | Once _ -> 0 
            | More (last, nextToLast) -> last - nextToLast
    (next, remember next turn history)

let initHistory startNumbers =
    let folder (turn, history) number =
        (turn + 1, history |> Map.add number (Once turn))
    let (_, history) = startNumbers |> List.fold folder (1, Map.empty)
    history

let getNumber endTurn startNumbers =
    let history = initHistory startNumbers
    let lastSpoken = List.last startNumbers
    let startTurn = 1 + List.length startNumbers
    let (number, _) = [startTurn .. endTurn] |> List.fold next (lastSpoken, history)
    number

[<EntryPoint>]
let main _ =
    let numbers = [1;0;15;2;10;13]
    getNumber 2020 numbers |> printfn "%d"
    getNumber 30000000 numbers |> printfn "%d"
    0 
