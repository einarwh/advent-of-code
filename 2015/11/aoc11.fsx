// Advent of Code 2015. Day 11: Corporate Policy.
// dotnet fsi aoc11.fsx

open System
open System.IO

// let findNext (s : string) = 
//     let skipTo (ch : char) (pos : int) (chars : char array) =
//         let getChar ix = 
//             if ix = pos then ch 
//             else if ix < pos then chars[ix]
//             else 'a'
//         [|0 .. chars.Length - 1|] |> Array.map getChar
//         chars
//     let nextWithoutIol (chars : char array) =
//         let rec fn pos =
//             if pos < chars.Length then 
//                 match chars[pos] with 
//                 | 'i' -> skipTo 'j' pos chars 
//                 | 'o' -> skipTo 'p' pos chars
//                 | 'l' -> skipTo 'm' pos chars
//                 | _ -> fn (pos + 1) 
//             else 
//                 chars
//         fn 0
//     let rec increment (pos : int) (chars : char array) = 
//         let ch = chars[pos]

//     let rec loop (chars : char array) = 
//         let cs = nextWithoutIol chars
//         let rec
//         // Increment


    
//     s

let nextChar c = if c = 'z' then 'a' else c |> int |> (+) 1 |> char 

let nextIncremental (s : string) = 
    let chars = s |> Seq.toArray
    let rec loop (pos : int) = 
        if pos < 0 then failwith "?"
        else 
            let c = chars[pos]
            if c = 'z' then 
                chars[pos] <- 'a'
                loop (pos - 1)
            else 
                chars[pos] <- nextChar c
    loop (chars.Length - 1)
    new string (chars)

let nextWithoutLetter (c : char) (s : string) = 
    // printfn "next without letter %c" c
    let ix = s.IndexOf c 
    // printfn "found %c in %s at index %d" c s ix 
    if ix < 0 then s 
    else 
        let before = s.Substring(0, ix) 
        // printfn "before '%s'" before 
        let afterLen = s.Length - (ix + 1)
        let after = new string('a', afterLen)
        // printfn "after '%s'" after
        before + (nextChar c).ToString() + after

let nextWithoutLetters (s : string) = 
    s |> nextWithoutLetter 'i' |> nextWithoutLetter 'o' |> nextWithoutLetter 'l'

let hasIncreasingSequence (s : string) = 
    let rec fn (chars : char list) = 
        match chars with 
        | a :: b :: c :: rest -> 
            let aVal = int a 
            let bVal = int b 
            let cVal = int c 
            if bVal - aVal = 1 && cVal - bVal = 1 then 
                true 
            else 
                fn (b :: c :: rest) 
        | _ -> false
    s |> Seq.toList |> fn 

let hasOverlappingPairs (s : string) = 
    let rec fn (count : int) (chars : char list) = 
        match chars with 
        | a :: b :: rest -> 
            if a = b then 
                if count = 1 then true 
                else 
                    fn (count + 1) rest
            else 
                fn count (b :: rest) 
        | _ -> false
    s |> Seq.toList |> fn 0 

let findNextPassword (s : string) = 
    let rec fn (s : string) = 
        // printfn "try find next password for %s" s
        let nxt = nextIncremental s |> nextWithoutLetters
        // printfn "next candidate %s" nxt
        if hasIncreasingSequence nxt && hasOverlappingPairs nxt then 
            nxt 
        else 
            fn nxt
    fn s

let run fileName = 
    let text = File.ReadAllText(fileName).Trim()
    // "abcdefgh" |> findNextPassword |> printfn "%s"
    // "ghijklmn" |> findNextPassword |> printfn "%s"
    let nxt1 = text |> findNextPassword
    let nxt2 = nxt1 |> findNextPassword
    nxt1 |> printfn "%s"
    nxt2 |> printfn "%s"
    // "abcdefgh" |> nextIncremental |> nextWithoutLetter 'i' |> printfn "%s"
    // "ghijklmn" |> nextIncremental |> nextWithoutLetter 'i' |> printfn "%s"

run "input.txt"
