// Advent of Code 2018. Day 13: Mine Cart Madness.
// dotnet fsi aoc13.fsx

open System
open System.IO

type Pos = int*int

type Cart = {
    symbol : char
    pos : Pos 
    switches : Map<Pos, int>
}

module Mine = 
    let width mine = 
        Array2D.length2 mine
    let height mine = 
        Array2D.length1 mine
    let get (mine : char[,]) (x, y) =
        Array2D.get mine y x
    let set (mine : char[,]) (x, y) (value : char) =
        Array2D.set mine y x value
    let count (mine : char[,]) = 
        let w = width mine
        let h = height mine
        let posList = [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        posList |> List.map (fun pos -> get mine pos) |> List.sum
    let fromList (lst : char list list) = 
        let width = lst |> List.head |> List.length 
        let height = lst |> List.length 
        Array2D.init height width (fun y x -> lst |> List.item y |> List.item x)    
    let toNestedList (mine : char[,]) = 
        let yRange = [ 0 .. mine.GetLength(0) - 1 ]
        let xRange = [ 0 .. mine.GetLength(1) - 1 ]
        yRange 
        |> List.map (fun y -> xRange |> List.map (fun x -> get mine (x, y)))
    let toIndexedList (mine : char[,]) = 
        let yRange = [ 0 .. mine.GetLength(0) - 1 ]
        let xRange = [ 0 .. mine.GetLength(1) - 1 ]
        yRange 
        |> List.map (fun y -> xRange |> List.map (fun x -> (x, y), get mine (x, y)))
        |> List.concat

let join (sep : string) (seq : string seq) = String.Join(sep, seq)

let visualize mine =
    let lines = Mine.toNestedList mine
    lines |> List.map (fun chars -> new String(List.toArray chars)) |> join "\n" |> printfn "%s"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let moveCart (mine : char[,]) (cart : Cart) = 
    let (x, y) = cart.pos 
    let nextPos = 
        match cart.symbol with 
        | '^' -> (x, y - 1)
        | '<' -> (x - 1, y)
        | 'v' -> (x, y + 1)
        | '>' -> (x + 1, y)
        | _ -> failwith <| sprintf "%c?" cart.symbol 
    let nextSymbol = 
        match Mine.get mine nextPos with 
        | '/' -> 
            match cart.symbol with 
            | '^' -> '>'
            | '<' -> 'v'
            | 'v' -> '<'
            | '>' -> '^'
            | _ -> failwith <| sprintf "%c?" cart.symbol 
        | '\\' -> 
            match cart.symbol with 
            | '^' -> '<'
            | '<' -> '^'
            | 'v' -> '>'
            | '>' -> 'v'
            | _ -> failwith <| sprintf "%c?" cart.symbol 
        | '+' -> 
            let history : int = cart.switches |> Map.find nextPos
            match cart.symbol with 
            | '^' -> 
                if history = 0 then '<'
                else if history = 1 then '^'
                else '>'
            | '<' -> 
                if history = 0 then 'v'
                else if history = 1 then '<'
                else '^'
            | 'v' ->
                if history = 0 then '>'
                else if history = 1 then 'v'
                else '<'
            | '>' ->
                if history = 0 then '^'
                else if history = 1 then '>'
                else 'v'
            | _ -> failwith <| sprintf "%c?" cart.symbol 
        | _ -> cart.symbol
    let nextSwitches = 
        match Mine.get mine nextPos with 
        | '+' -> 
            cart.switches |> Map.change nextPos (Option.map (fun h -> (h + 1) % 3))
        | _ -> 
            cart.switches
    { symbol = nextSymbol; pos = nextPos; switches = nextSwitches }

let run fileName = 
    let lines = readLines fileName
    lines |> List.iter (printfn "%s")
    let mine = lines |> List.map Seq.toList |> Mine.fromList
    printfn "%A" mine
    mine |> visualize
    let indexed = mine |> Mine.toIndexedList 
    let isSwitch ch = ch = '+'
    let isCart ch = ['^'; '<'; 'v'; '>'] |> List.contains ch
    let switches = indexed |> List.choose (fun (pos, ch) -> if isSwitch ch then Some pos else None)
    let switchMap = switches |> List.map (fun s -> (s, 0)) |> Map.ofList
    printfn "%d" <| List.length switches
    let carts = indexed |> List.choose (fun (pos, ch) -> if isCart ch then Some { symbol = ch; pos = pos; switches = switchMap } else None)
    printfn "%A" carts
    printfn "%d" <| List.length carts

run "sample.txt"
