// Advent of Code 2015. Day 11: Corporate Policy.
// dotnet fsi aoc11.fsx

open System
open System.IO

let findNext (s : string) = 
    let skipTo (ch : char) (pos : int) (chars : char array) =
        let getChar ix = 
            if ix = pos then ch 
            else if ix < pos then chars[ix]
            else 'a'
        [|0 .. chars.Length - 1|] |> Array.map getChar
        chars
    let nextWithoutIol (chars : char array) =
        let rec fn pos =
            if pos < chars.Length then 
                match chars[pos] with 
                | 'i' -> skipTo 'j' pos chars 
                | 'o' -> skipTo 'p' pos chars
                | 'l' -> skipTo 'm' pos chars
                | _ -> fn (pos + 1) 
            else 
                chars
        fn 0
    let rec loop (chars : char array) = 
        let cs = nextWithoutIol chars
        let rec
        // Increment


    
    s


let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    text |> printfn "%s"

run "input.txt"
