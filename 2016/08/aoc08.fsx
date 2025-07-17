// Advent of Code 2016. Day 08: Two-Factor Authentication.
// dotnet fsi aoc08.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Operation = 
    | Rect of int*int 
    | Row of int*int
    | Column of int*int

module Screen = 
    let create width height = 
        Array2D.create height width false
    let width screen = 
        Array2D.length2 screen
    let height screen = 
        Array2D.length1 screen
    let get (screen : bool[,]) (x, y) =
        Array2D.get screen y x
    let set (screen : bool[,]) (x, y) (value : bool) =
        Array2D.set screen y x value
    let rect (screen : bool[,]) (w, h) = 
        let rectPositions = 
            [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        let rec fn posList = 
            match posList with 
            | [] -> ()
            | pos :: rest -> 
                set screen pos true 
                fn rest
        fn rectPositions
    let rotateColumn (screen : bool[,]) (x : int) (steps : int) = 
        let h = height screen
        let column = [|0 .. h-1|] |> Array.map (fun y -> get screen (x, y))
        let rot (y : int) =
            if y < Array.length column then 
                let v = Array.get column y 
                let y' =  (y + steps) % h 
                set screen (x, y') v
        [0 .. h-1] |> List.iter rot
    let rotateRow (screen : bool[,]) (y : int) (steps : int) = 
        let w = width screen
        let row = [|0 .. w-1|] |> Array.map (fun x -> get screen (x, y))
        let rot (x : int) =
            if x < Array.length row then 
                let v = Array.get row x
                let x' =  (x + steps) % w
                set screen (x', y) v
        [0 .. w-1] |> List.iter rot
    let countLit (screen : bool[,]) = 
        let w = width screen
        let h = height screen
        let posList = [for x in [0..w-1] do for y in [0..h-1] -> (x, y)]
        posList |> List.filter (fun pos -> get screen pos) |> List.length

let tryParseRect (s : string) : Operation option = 
    let m = Regex.Match(s, "^rect (\d+)x(\d+)$")
    if m.Success then
        let w = m.Groups.[1].Value |> int 
        let h = m.Groups.[2].Value |> int 
        Some <| Rect (w, h)
    else
        None

let tryParseRow (s : string) : Operation option = 
    let m = Regex.Match(s, "^rotate row y=(\d+) by (\d+)$")
    if m.Success then
        let y = m.Groups.[1].Value |> int 
        let r = m.Groups.[2].Value |> int 
        Some <| Row (y, r)
    else
        None

let tryParseColumn (s : string) : Operation option = 
    let m = Regex.Match(s, "^rotate column x=(\d+) by (\d+)$")
    if m.Success then
        let x = m.Groups.[1].Value |> int 
        let r = m.Groups.[2].Value |> int 
        Some <| Column (x, r)
    else
        None

let tryParse (s : string) : Operation option = 
    tryParseRect s
    |> Option.orElse (tryParseRow s)
    |> Option.orElse (tryParseColumn s)

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let visualize screen = 
    let w = Screen.width screen 
    let h = Screen.height screen
    let createRow y = 
        [ 0 .. (w - 1) ] 
        |> List.map (fun x -> if Screen.get screen (x, y) then "#" else ".")
        |> String.concat ""
    [ 0 .. (h - 1) ] 
    |> List.map createRow |> String.concat "\n" |> printfn "%s"
    printfn ""

let execute screen operations = 
    let rec fn ops = 
        match ops with 
        | [] -> () 
        | op :: rest ->
            match op with 
            | Rect (w, h) -> Screen.rect screen (w, h)
            | Row (y, steps) -> Screen.rotateRow screen y steps
            | Column (x, steps) -> Screen.rotateColumn screen x steps 
            fn rest 
    fn operations

let run fileName = 
    let lines = readLines fileName
    let operations = lines |> List.choose tryParse
    let screen = Screen.create 50 6 
    execute screen operations
    Screen.countLit screen |> printfn "%d"
    visualize screen

run "input"
