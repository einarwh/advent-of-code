// Advent of Code 2016. Day 21: Scrambled Letters and Hash.
// dotnet fsi aoc21.fsx

open System
open System.IO
open System.Text.RegularExpressions

type Operation =
    | SwapLetters of (char * char)
    | SwapPositions of (int * int)
    | RotateLeft of int
    | RotateRight of int
    | RotatePosition of char
    | ReversePositions of (int * int)
    | MovePosition of (int * int)

let tryParseSwapPositions (s : string) : Operation option =
    let m = Regex.Match(s, "^swap position (\d+) with position (\d+)$")
    if m.Success then
        let pos1 = m.Groups[1].Value |> int
        let pos2 = m.Groups[2].Value |> int
        Some <| SwapPositions (pos1, pos2)
    else
        None

let tryParseSwapLetters (s : string) : Operation option =
    let m = Regex.Match(s, "^swap letter ([a-z]) with letter ([a-z])$")
    if m.Success then
        let ch1 = m.Groups[1].Value[0]
        let ch2 = m.Groups[2].Value[0]
        Some <| SwapLetters (ch1, ch2)
    else
        None

let tryParseRotateLeft (s : string) : Operation option =
    let m = Regex.Match(s, "^rotate left (\d+) step")
    if m.Success then
        let steps = m.Groups[1].Value |> int
        Some <| RotateLeft steps
    else
        None

let tryParseRotateRight (s : string) : Operation option =
    let m = Regex.Match(s, "^rotate right (\d+) step")
    if m.Success then
        let steps = m.Groups[1].Value |> int
        Some <| RotateRight steps
    else
        None

let tryParseRotatePosition (s : string) : Operation option =
    let m = Regex.Match(s, "^rotate based on position of letter ([a-z])$")
    if m.Success then
        let ch = m.Groups[1].Value[0]
        Some <| RotatePosition ch
    else
        None

let tryParseReversePositions (s : string) : Operation option =
    let m = Regex.Match(s, "^reverse positions (\d+) through (\d+)$")
    if m.Success then
        let pos1 = m.Groups[1].Value |> int
        let pos2 = m.Groups[2].Value |> int
        Some <| ReversePositions (pos1, pos2)
    else
        None

let tryParseMovePosition (s : string) : Operation option =
    let m = Regex.Match(s, "^move position (\d+) to position (\d+)$")
    if m.Success then
        let pos1 = m.Groups.[1].Value |> int
        let pos2 = m.Groups.[2].Value |> int
        Some <| MovePosition (pos1, pos2)
    else
        None

let tryParse (s : String) : Operation option =
   s
   |> tryParseSwapPositions
   |> Option.orElseWith (fun () -> tryParseSwapLetters s)
   |> Option.orElseWith (fun () -> tryParseRotateLeft s)
   |> Option.orElseWith (fun () -> tryParseRotateRight s)
   |> Option.orElseWith (fun () -> tryParseRotatePosition s)
   |> Option.orElseWith (fun () -> tryParseReversePositions s)
   |> Option.orElseWith (fun () -> tryParseMovePosition s)
   |> Option.orElseWith (fun () -> failwith <| sprintf "Failed to parse '%s'!" s)

let swapLetters ch1 ch2 password =
    let rec loop pwd =
        match pwd with
        | [] -> []
        | ch :: rest when ch = ch1 -> ch2 :: loop rest
        | ch :: rest when ch = ch2 -> ch1 :: loop rest
        | ch :: rest -> ch :: loop rest
    loop password

let swapPositions pos1 pos2 password =
    let ch1 = password |> List.item pos1
    let ch2 = password |> List.item pos2
    swapLetters ch1 ch2 password

let rec rotateLeft steps password =
    if steps > 0 then
        match password with
        | [] -> []
        | h :: t -> rotateLeft (steps - 1) (t @ [h])
    else
        password

let rotateRight steps password =
    let reversed = password |> List.rev
    let rotated = rotateLeft steps reversed
    rotated |> List.rev

let rotatePosition ch password =
    let ix = password |> List.indexed |> List.find (fun (_, c) -> c = ch) |> fst
    let steps = if ix >= 4 then ix + 2 else ix + 1
    rotateRight steps password

let unrotatePosition ch password =
    let ix = password |> List.indexed |> List.find (fun (_, c) -> c = ch) |> fst
    let reversedIx =
        if ix = 0 then 
            (2 * List.length password - 2) / 2
        else if ix % 2 = 0 then 
            (ix + List.length password - 2) / 2
        else 
            (ix - 1) / 2
    let steps = if reversedIx >= 4 then reversedIx + 2 else reversedIx + 1 
    rotateLeft steps password 

let reversePositions pos1 pos2 password =
    let section = password |> List.take (pos2 + 1) |> List.skip pos1
    let reversed = section |> List.rev
    let before = List.truncate pos1 password
    let after = List.skip (pos2 + 1) password
    before @ reversed @ after

let movePosition pos1 pos2 password =
    let ch = List.item pos1 password
    password |> List.removeAt pos1 |> List.insertAt pos2 ch

let scramble op password =
    match op with
    | SwapPositions (pos1, pos2) -> swapPositions pos1 pos2 password
    | SwapLetters (ch1, ch2) -> swapLetters ch1 ch2 password
    | RotateLeft steps -> rotateLeft steps password
    | RotateRight steps -> rotateRight steps password
    | RotatePosition ch -> rotatePosition ch password
    | ReversePositions (pos1, pos2) -> reversePositions pos1 pos2 password
    | MovePosition (pos1, pos2) -> movePosition pos1 pos2 password

let unscramble op password =
    match op with
    | SwapPositions (pos1, pos2) -> swapPositions pos1 pos2 password
    | SwapLetters (ch1, ch2) -> swapLetters ch1 ch2 password
    | RotateLeft steps -> rotateRight steps password
    | RotateRight steps -> rotateLeft steps password
    | RotatePosition ch -> unrotatePosition ch password
    | ReversePositions (pos1, pos2) -> reversePositions pos1 pos2 password
    | MovePosition (pos1, pos2) -> movePosition pos2 pos1 password

let scrambleAll (password : string) operations =
    let rec loop pwd ops =
        match ops with
        | [] -> pwd
        | op :: rest ->
            loop (scramble op pwd) rest
    let scrambled = loop (password |> Seq.toList) operations
    new string(scrambled |> List.toArray)

let unscrambleAll (password : string) operations =
    let rec loop pwd ops =
        match ops with
        | [] -> pwd
        | op :: rest ->
            loop (unscramble op pwd) rest
    let scrambled = loop (password |> Seq.toList) operations
    new string(scrambled |> List.toArray)

let readLines =
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let run fileName =
    let lines = readLines fileName
    let operations = lines |> List.choose tryParse
    let password = "abcdefgh"
    let scrambled = "fbgdceah"
    scrambleAll password operations |> printfn "%s"
    unscrambleAll scrambled (List.rev operations) |> printfn "%s"

run "input.txt"
