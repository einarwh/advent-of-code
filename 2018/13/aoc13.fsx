// Advent of Code 2018. Day 13: Mine Cart Madness.
// dotnet fsi aoc13.fsx

open System
open System.IO

type Pos = int*int

type Cart = {
    symbol : char
    pos : Pos
    switches : int
}

module Mine =
    let get (mine : char[,]) (x, y) =
        Array2D.get mine y x
    let set (mine : char[,]) (x, y) (value : char) =
        Array2D.set mine y x value
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

let visualize carts mine =
    let viz = Array2D.copy mine
    let chooseSymbol pos symbol =
        let cartsAtPos = carts |> List.filter (fun c -> c.pos = pos) |> List.length
        if cartsAtPos > 1 then 'X' else symbol
    carts |> List.iter (fun c -> Mine.set viz c.pos (chooseSymbol c.pos c.symbol))
    let lines = viz |> Mine.toNestedList
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
            let history = cart.switches
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
    let nextSwitches : int =
        match Mine.get mine nextPos with
        | '+' ->
            (cart.switches + 1) % 3
        | _ ->
            cart.switches
    { symbol = nextSymbol; pos = nextPos; switches = nextSwitches }

let rec crash (removeCrashed : bool) (mine : char[,]) (carts : Cart list) : int*int=
    // visualize carts mine
    let cartsAtPositions =
        carts
        |> List.groupBy (fun c -> c.pos)
        |> List.sortByDescending (fun (_, carts) -> carts.Length)
    let crashes = cartsAtPositions |> List.choose (fun (_, cs) -> if cs.Length > 1 then Some cs else None) |> List.concat
    if crashes.Length > 1 then
        if removeCrashed then
            let remaining = carts |> List.filter (fun c -> not <| List.contains c crashes)
            if remaining.Length > 1 then
                remaining |> List.map (moveCart mine) |> crash removeCrashed mine
            else
                remaining |> List.head |> fun c -> c.pos
        else
            crashes |> List.head |> fun c -> c.pos
    else
        carts |> List.map (moveCart mine) |> crash removeCrashed mine

let run fileName =
    let lines = readLines fileName
    let mine = lines |> List.map Seq.toList |> Mine.fromList
    let indexed = mine |> Mine.toIndexedList
    let isCart ch = ['^'; '<'; 'v'; '>'] |> List.contains ch
    let carts = indexed |> List.choose (fun (pos, ch) -> if isCart ch then Some { symbol = ch; pos = pos; switches = 0 } else None)
    let cartReplacementSymbol symbol =
        match symbol with
        | '^' | 'v' -> '|'
        | '>' | '<' -> '-'
        | _ -> symbol
    carts |> List.iter (fun c -> Mine.set mine c.pos (cartReplacementSymbol c.symbol))
    carts |> crash false mine |> fun (x, y) -> printfn "%d,%d" x y
    carts |> crash true mine |> fun (x, y) -> printfn "%d,%d" x y

run "input.txt"
