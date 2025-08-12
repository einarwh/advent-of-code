// Advent of Code 2020. Day 22: Crab Combat. Part A.
// dotnet fsi aoc22a.fsx

open System.IO

let trim (input : string) = input.Trim()

let split (splitter : string) (input : string) = input.Split(splitter)

let parsePlayer (line : string) : int list = 
    match line |> trim |> split "\n" |> Array.toList with 
    | [] -> failwith "empty input"
    | _ :: t -> t |> List.map int 

let endGame winningCards : int = 
    winningCards |> List.rev |> List.mapi (fun i c -> (i + 1) * c) |> List.sum

let rec playGame (round : int) (cards1 : int list) (cards2 : int list) = 
    match (cards1, cards2) with 
    | [], _ -> 
        (0, endGame cards2)
    | _, [] -> 
        (endGame cards1, 0)
    | h1 :: t1, h2 :: t2 -> 
        let round' = round + 1
        if h1 > h2 then 
            playGame round' (t1 @ [h1; h2]) t2
        else 
            playGame round' t1 (t2 @ [h2; h1])

let serializeCards (cards : int list) = 
    cards |> List.map string |> String.concat ", "

let rec playRecursiveGame (game : int) (round : int) (seen : Set<int list * int list>) (cards1 : int list) (cards2 : int list) : (int * int) = 
    if Set.contains (cards1, cards2) seen then 
        (endGame cards1, 0)
    else 
        match (cards1, cards2) with 
        | [], _ -> 
            (0, endGame cards2)
        | _, [] -> 
            (endGame cards1, 0)
        | c1 :: t1, c2 :: t2 -> 
            // printfn "-- Round %d (Game %d) --" round game  
            // printfn "Player 1's deck: %s" (serializeCards cards1) 
            // printfn "Player 2's deck: %s" (serializeCards cards2) 
            let round' = round + 1
            let seen' = Set.add (cards1, cards2) seen 
            if c1 <= List.length t1 && c2 <= List.length t2 then 
                // printfn "Playing a sub-game to determine the winner..."
                // printfn ""
                // Recur.
                let nextCards1 = t1 |> List.take c1 
                let nextCards2 = t2 |> List.take c2 
                let (score1, score2) = playRecursiveGame (game + 1) 1 Set.empty nextCards1 nextCards2 
                if score1 > score2 then 
                    // printfn "The winner of %d is player 1!" (game + 1)
                    // printfn ""
                    // printfn "...anyway, back to game %d" game
                    // printfn "Player 1 wins round %d of game %d!" round game 
                    playRecursiveGame game round' seen' (t1 @ [c1; c2]) t2
                else 
                    // printfn "The winner of %d is player 2!" (game + 1)
                    // printfn ""
                    // printfn "...anyway, back to game %d" game
                    // printfn "Player 2 wins round %d of game %d!" round game 
                    playRecursiveGame game round' seen' t1 (t2 @ [c2; c1])
            else 
                // printfn "Player 1 plays: %d" c1
                // printfn "Player 2 plays: %d" c2
                if c1 > c2 then 
                    // printfn "Player 1 wins round %d of game %d!" round game 
                    // printfn ""
                    playRecursiveGame game round' seen' (t1 @ [c1; c2]) t2
                else 
                    // printfn "Player 2 wins round %d of game %d!" round game 
                    // printfn ""
                    playRecursiveGame game round' seen' t1 (t2 @ [c2; c1])

let run (cardArray : int list array) =
    let cards1 = cardArray[0]
    let cards2 = cardArray[1]
    playGame 1 cards1 cards2 |> printfn "%A"
    playRecursiveGame 1 1 Set.empty cards1 cards2 |> printfn "%A"

"input.txt"
|> File.ReadAllText
|> split "\n\n"
|> Array.map parsePlayer
|> run
