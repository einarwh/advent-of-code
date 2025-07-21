// Advent of Code 2018. Day 13: Mine Cart Madness.
// dotnet fsi aoc13.fsx

open System
open System.IO

type Pos = int*int

type Cart = {
    pos : Pos 
    switches : Map<Pos, int>
}


module Mine = 
    let create width height = 
        Array2D.create height width 0
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
    let toList (mine : char[,]) = 
        let yRange = [ 0 .. mine.GetLength(0) - 1 ]
        let xRange = [ 0 .. mine.GetLength(1) - 1 ]
        yRange 
        |> List.map (fun y -> xRange |> List.map (fun x -> get mine (x, y)))

let join (sep : string) (seq : string seq) = String.Join(sep, seq)

let visualize mine =
    let lines = Mine.toList mine
    lines |> List.map (fun chars -> new String(List.toArray chars)) |> join "\n" |> printfn "%s"

let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName = 
    let lines = readLines fileName
    lines |> List.iter (printfn "%s")
    let mine = lines |> List.map Seq.toList |> Mine.fromList
    printfn "%A" mine
    mine |> visualize

run "sample.txt"
