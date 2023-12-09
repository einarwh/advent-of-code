// Advent of Code 2018. Day 3: No Matter How You Slice It
// dotnet fsi aoc02.fsx

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections 

type Claim = {
    Id : int 
    XOffset : int
    YOffset : int 
    Width : int
    Height : int  
}

let parseNumbers (s : string) : int list = 
    Regex.Matches(s, "\d+") 
    |> Seq.map (fun m -> int m.Value)
    |> Seq.toList 

let parseClaim (s : string) : Claim = 
    match parseNumbers s with 
    | [c;x;y;w;h] -> 
        { Id = c; XOffset = x; YOffset = y; Width = w; Height = h }
    | _ -> failwith <| sprintf "Failed to parse %s" s 

let readLines = 
    File.ReadAllLines >> Array.filter ((<>) String.Empty)

let increment array (index1, index2) =
    let n = Array2D.get array index1 index2 
    Array2D.set array index1 index2 (n + 1)

let rec applyClaims square (claims : Claim list) = 
    let rec loop claims = 
        match claims with 
        | [] -> ()
        | c :: rest -> 
            let xs = [c.XOffset .. c.XOffset + c.Width - 1]
            let ys = [c.YOffset .. c.YOffset + c.Height - 1]
            [for y in ys do for x in xs do yield (x,y)]
            |> List.iter (increment square)
            loop rest
    loop claims

let toSequence (a:'a[,]) : seq<'a> =
    seq { for i in 0 .. a.GetLength(0)-1 do
          for j in 0 .. a.GetLength(1)-1 do yield a.[i,j] }

let noOverlap square (c : Claim) = 
    let xs = [c.XOffset .. c.XOffset + c.Width - 1]
    let ys = [c.YOffset .. c.YOffset + c.Height - 1]
    [for y in ys do for x in xs do yield (x,y)]
    |> List.map (fun (x, y) -> Array2D.get square x y)
    |> List.forall ((=) 1)

let run fileName = 
    let lines = readLines fileName |> Array.toList
    let claims = lines |> List.map parseClaim
    let square = Array2D.create 1000 1000 0
    claims |> applyClaims square
    square 
    |> toSequence 
    |> Seq.filter ((<) 1) 
    |> Seq.length 
    |> printfn "%d"
    claims 
    |> List.filter (noOverlap square) 
    |> List.head
    |> (fun c -> c.Id)
    |> printfn "%A"

"input" |> run 
