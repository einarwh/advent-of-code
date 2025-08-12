// Advent of Code 2020. Day 23: Crab Cups.
// dotnet fsi aoc23.fsx

open System
open System.IO

type Circle = {
    current : int 
    cups : int array
}

module Circle = 

    let getIndex offset index cupCount = 
        (index + offset) % cupCount

    let getDestinationCup current remaining removed = 
        let rec loop candidate = 
            if candidate < 1 then loop (Array.max remaining)
            else
                if Array.contains candidate removed then 
                    loop (candidate - 1)
                else 
                    candidate 
        loop (current - 1)

    let move (circle : Circle) = 
        let index = circle.cups |> Array.findIndex (fun c -> c = circle.current)
        let cupCount = circle.cups |> Array.length 
        let i1 = (index + 1) % cupCount
        let i2 = (index + 2) % cupCount
        let i3 = (index + 3) % cupCount
        let cup1 = circle.cups[i1]
        let cup2 = circle.cups[i2]
        let cup3 = circle.cups[i3]
        let removed = [|cup1;cup2;cup3|]
        let cupsStr = circle.cups |> Array.toList |> List.map (fun c -> if c = circle.current then "(" + string c + ")" else " " + string c + " ") |> String.concat " "
        // printfn "cups: %s" cupsStr
        // printfn "current: %d" circle.current
        let removedStr = removed |> Array.toList |> List.map string |> String.concat ", "
        // printfn "pick up: %s" removedStr
        let remaining = circle.cups |> Array.filter (fun c -> not <| Array.contains c removed)
        let destination = getDestinationCup circle.current remaining removed 
        // printfn "destination: %d" destination
        let destinationIndex = Array.findIndex (fun c -> c = destination) remaining
        // printfn "destination index: %d" destinationIndex
        let splitIndex = destinationIndex + 1
        let movedCups = 
            if splitIndex < cupCount then 
                let left, right = Array.splitAt splitIndex remaining
                Array.concat [| left; removed; right |]
            else 
                Array.concat [| remaining; removed |]
        let movedIndex = movedCups |> Array.findIndex (fun c -> c = circle.current)
        let nextCup = movedCups[(movedIndex + 1) % cupCount]
        { current = nextCup; cups = movedCups }

    let findOrder circle = 
        let cups = circle.cups 
        let ix = Array.findIndex (fun c -> c = 1) cups
        if ix = 0 then cups[1..]
        else if ix = Array.length cups - 1 then cups[..(Array.length cups - 1)]
        else 
            let before = cups[0..ix-1]
            let after = cups[ix+1..Array.length cups - 1]
            Array.concat [|after; before|]

    let moveLoop moves circle = 
        let rec loop i c = 
            // printfn "\n-- move %d --" i 
            if i <= moves then 
                loop (i + 1) (move c) 
            else 
                c |> findOrder |> Array.toList |> List.map string |> String.concat ""
        loop 1 circle

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    let cups : int array = text |> Seq.toArray |> Array.map (fun ch -> Int32.Parse(ch.ToString()))
    let circle = { current = cups[0]; cups = cups }
    Circle.moveLoop 100 circle |> printfn "%s"

run "input.txt"
