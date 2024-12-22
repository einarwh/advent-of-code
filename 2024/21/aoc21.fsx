// Advent of Code 2024. Day 21
// dotnet fsi aoc21.fsx

open System
open System.IO

let lookupNumericKeypad (source, target) : string= 
    match (source, target) with 
    | ('A', '0') -> "<A"
    | ('A', '1') -> "^<<A"
    | ('A', '2') -> "^<A"
    | ('A', '3') -> "^A"
    | ('A', '4') -> "^^<<A"
    | ('A', '5') -> "^^<A"
    | ('A', '6') -> "^^A"
    | ('A', '7') -> "^^^<<A"
    | ('A', '8') -> "^^^<A"
    | ('A', '9') -> "^^^A"

    | ('0', 'A') -> ">A" 
    | ('0', '1') -> "^<A"
    | ('0', '2') -> "^A"
    | ('0', '3') -> "^>A"
    | ('0', '4') -> "^^<A"
    | ('0', '5') -> "^^A"
    | ('0', '6') -> "^^>A"
    | ('0', '7') -> "^^^<A"
    | ('0', '8') -> "^^^A"
    | ('0', '9') -> "^^^>A"

    | ('1', 'A') -> ">>vA"
    | ('1', '0') -> ">vA"
    | ('1', '2') -> ">A"
    | ('1', '3') -> ">>A"
    | ('1', '4') -> "^A"
    | ('1', '5') -> "^>A"
    | ('1', '6') -> "^>>A"
    | ('1', '7') -> "^^A"
    | ('1', '8') -> "^^>A"
    | ('1', '9') -> "^^>>A"

    | ('2', 'A') -> ">vA"
    | ('2', '0') -> "vA"
    | ('2', '1') -> "<A"
    | ('2', '3') -> ">A"
    | ('2', '4') -> "^<A"
    | ('2', '5') -> "^A"
    | ('2', '6') -> "^>A"
    | ('2', '7') -> "^^<A"
    | ('2', '8') -> "^^A"
    | ('2', '9') -> "^^>A"

// +---+---+---+
// | 7 | 8 | 9 |
// +---+---+---+
// | 4 | 5 | 6 |
// +---+---+---+
// | 1 | 2 | 3 |
// +---+---+---+
//     | 0 | A |
//     +---+---+

    | ('3', 'A') -> "vA"
    | ('3', '0') -> "<vA"
    | ('3', '1') -> "<<A"
    | ('3', '2') -> "<A"
    | ('3', '4') -> "^<<A"
    | ('3', '5') -> "^<A"
    | ('3', '6') -> "^A"
    | ('3', '7') -> "^^<<A"
    | ('3', '8') -> "^^<A"
    | ('3', '9') -> "^^A"

    | ('4', 'A') -> ">>vvA"
    | ('4', '0') -> ">vvA"
    | ('4', '1') -> "vA"
    | ('4', '2') -> ">vA"
    | ('4', '3') -> ">>vA"
    | ('4', '5') -> ">A"
    | ('4', '6') -> ">>A"
    | ('4', '7') -> "^A"
    | ('4', '8') -> "^>A"
    | ('4', '9') -> "^>>A"

    | ('5', 'A') -> ">vvA"
    | ('5', '0') -> "vvA"
    | ('5', '1') -> "<vA"
    | ('5', '2') -> "vA"
    | ('5', '3') -> ">vA"
    | ('5', '4') -> "<A"
    | ('5', '6') -> ">A"
    | ('5', '7') -> "^<A"
    | ('5', '8') -> "^A"
    | ('5', '9') -> "^>A"

    | ('6', 'A') -> "vvA"
    | ('6', '0') -> "<vvA"
    | ('6', '1') -> "<<vA"
    | ('6', '2') -> "<vA"
    | ('6', '3') -> "vA"
    | ('6', '4') -> "<<A"
    | ('6', '5') -> "<A"
    | ('6', '7') -> "^<<A"
    | ('6', '8') -> "^<A"
    | ('6', '9') -> "^A"

    | ('7', 'A') -> ">>vvvA"
    | ('7', '0') -> ">vvvA"
    | ('7', '1') -> "vvA"
    | ('7', '2') -> ">vvA"
    | ('7', '3') -> ">>vvA"
    | ('7', '4') -> "vA"
    | ('7', '5') -> ">vA"
    | ('7', '6') -> ">>vA"
    | ('7', '8') -> ">A"
    | ('7', '9') -> ">>A"

    | ('8', 'A') -> ">vvvA"
    | ('8', '0') -> "vvvA"
    | ('8', '1') -> "<vvA"
    | ('8', '2') -> "vvA"
    | ('8', '3') -> ">vvA"
    | ('8', '4') -> "<vA"
    | ('8', '5') -> "vA"
    | ('8', '6') -> ">vA"
    | ('8', '7') -> "<A"
    | ('8', '9') -> ">A"

    | ('9', 'A') -> "vvvA"
    | ('9', '0') -> "<vvvA"
    | ('9', '1') -> "<<vvA"
    | ('9', '2') -> "<vvA"
    | ('9', '3') -> "vvA"
    | ('9', '4') -> "<<vA"
    | ('9', '5') -> "<vA"
    | ('9', '6') -> "vA"
    | ('9', '7') -> "<<A"
    | ('9', '8') -> "<A"

    | _ -> "A"

let lookupDirectionalKeypad (source, target) = 
    match (source, target) with 
    | ('A', '^') -> "<A"
    | ('A', '<') -> "v<<A"
    | ('A', 'v') -> "v<A"
    | ('A', '>') -> "vA"

    | ('^', 'A') -> ">A"
    | ('^', '<') -> "v<A"
    | ('^', 'v') -> "vA"
    | ('^', '>') -> "v>A"

    | ('<', 'A') -> ">>^A"
    | ('<', '^') -> ">^A"
    | ('<', 'v') -> ">A"
    | ('<', '>') -> ">>A"
    
    | ('v', 'A') -> ">^A"
    | ('v', '^') -> "^A"
    | ('v', '<') -> "<A"
    | ('v', '>') -> ">A"

    | ('>', 'A') -> "^A"
    | ('>', '^') -> "<^A"
    | ('>', '<') -> "<<A"
    | ('>', 'v') -> "<A"

    | _ -> "A"
    
let readLines = 
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let prependA s = "A" + s

let rec getKeypresses lookups code = 
    match lookups with 
    | [] -> code 
    | lookup :: rest -> 
        let result = 
            code 
            |> prependA 
            |> Seq.toList 
            |> List.pairwise 
            |> List.map lookup 
            |> String.concat ""
        printfn "%s" result
        result |> getKeypresses rest 

let parseNumeric (code : string) = 
    code.Substring(0, 3) |> int 

let complexity code = 
    let numeric = code |> parseNumeric
    let lookups = [ lookupNumericKeypad; lookupDirectionalKeypad; lookupDirectionalKeypad ]
    let keypresses = code |> getKeypresses lookups 
    printfn "%s: %s" code keypresses
    let length = keypresses |> String.length 
    // printfn "(%d * %d)" length numeric 
    numeric * length 

let run fileName = 
    let codes = readLines fileName
    codes |> printfn "%A"
    codes |> List.map complexity |> List.sum |> printfn "%d"
    // "029A" |> complexity |> printfn "%A"
    // code |> getKeypresses lookups |> printfn "%s"

    // code 
    // |> prependA 
    // |> Seq.toList 
    // |> List.pairwise 
    // |> List.map lookupNumericKeypad 
    // |> String.concat ""
    // |> prependA 
    // |> Seq.toList 
    // |> List.pairwise 
    // |> List.map lookupDirectionalKeypad
    // |> String.concat ""
    // |> printfn "%s"
    // // code |> Seq.toList |> List.pairwise |> List.map lookupNumericKeypad |> printfn "%A"
    // "<A^A^^>AvvvA" |> prependA |> Seq.toList |> List.pairwise |> printfn "%A"
    0

run "sample"

// <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
// v<<A>>^A<A>AvA<^AA>A<vAAA>^A
// <A ^A >^^A vvvA
// 029A

// <A ^A ^^>A vvvA
// v<<A>>^A<A>A<AAv>A^Av<AAA>^A
// v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AAv<A>A^A<A>Av<A<A>>^AAAvA<^A>A

// 029A: v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AAv<A>A^A<A>Av<A<A>>^AAAvA<^A>A


// <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
// v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AAv<A>A^A<A>Av<A<A>>^AAAvA<^A>A
// 029A

// 029A: v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AAv<A>A^A<A>Av<A<A>>^AAAvA<^A>A
// 980A: v<<A>>^AAAvA^Av<A<AA>>^AvAA<^A>Av<A<A>>^AAAvA<^A>Av<<A>>^AvA^A
// 980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
// 179A: v<<A>>^Av<A<A>>^AAvAA<^A>Av<<A>>^AAvA^Av<A>^AA<A>Av<A<A>>^AAAvA<^A>A
// 456A: v<<A>>^AAv<A<A>>^AAvAA<^A>Av<A>^A<A>Av<A>^A<A>Av<A<A>>^AAvA<^A>A
// 379A: v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA<^A>Av<A>^AA<A>Av<A<A>>^AAAvA<^A>A


// 029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
// 179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
// 456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
// 379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A