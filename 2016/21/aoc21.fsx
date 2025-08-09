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

let asStr (chars : char list) =
    new string(chars |> List.toArray)

let swapLetters ch1 ch2 password =
    // printfn "\nswap letters %c <-> %c in %s" ch1 ch2 (asStr password)
    let rec loop pwd =
        match pwd with
        | [] -> []
        | ch :: rest when ch = ch1 -> ch2 :: loop rest
        | ch :: rest when ch = ch2 -> ch1 :: loop rest
        | ch :: rest -> ch :: loop rest
    let result = loop password
    // printfn "%s -> %s" (asStr password) (asStr result)
    result

let swapPositions pos1 pos2 password =
    // printfn "\nswap positions %d <-> %d in %s" pos1 pos2 (new string(password|> List.toArray))
    let ch1 = password |> List.item pos1
    let ch2 = password |> List.item pos2
    let result = swapLetters ch1 ch2 password
    // printfn "%s -> %s" (asStr password) (asStr result)
    result

let rec rotateLeft steps password =
    // printfn "\nrotate left %d steps: %s" steps (asStr password)
    let result =
        if steps > 0 then
            match password with
            | [] -> []
            | h :: t -> rotateLeft (steps - 1) (t @ [h])
        else
            password
    // printfn "%s -> %s" (asStr password) (asStr result)
    result

let rotateRight steps password =
    // printfn "\nrotate right %d steps: %s" steps (asStr password)
    let reversed = password |> List.rev
    let rotated = rotateLeft steps reversed
    let result = rotated |> List.rev
    // printfn "%s -> %s" (asStr password) (asStr result)
    result

let rotatePosition ch password =
    // printfn "\nrotate based on position of %c in %s " ch (asStr password)
    let ix = password |> List.indexed |> List.find (fun (_, c) -> c = ch) |> fst
    // printfn "index: %d" ix
    let steps = if ix >= 4 then ix + 2 else ix + 1
    // printfn "steps: %d" steps
    let result = rotateRight steps password
    // printfn "%s -> %s" (asStr password) (asStr result)
    result

let reversePositions pos1 pos2 password =
    // printfn "\nreverse positions [%d - %d] in %s " pos1 pos2 (asStr password)
    let section = password |> List.take (pos2 + 1) |> List.skip pos1
    let reversed = section |> List.rev
    let before = List.truncate pos1 password
    let after = List.skip (pos2 + 1) password
    let result = before @ reversed @ after
    // printfn "%s -> %s" (asStr password) (asStr result)
    result

let movePosition pos1 pos2 password =
    // printfn "\nmove position letter at %d to %d in %s" pos1 pos2 (asStr password)
    let ch = List.item pos1 password
    let result = password |> List.removeAt pos1 |> List.insertAt pos2 ch
    // printfn "%s -> %s" (asStr password) (asStr result)
    result

let scramble password op =
    match op with
    | SwapPositions (pos1, pos2) -> swapPositions pos1 pos2 password
    | SwapLetters (ch1, ch2) -> swapLetters ch1 ch2 password
    | RotateLeft steps -> rotateLeft steps password
    | RotateRight steps -> rotateRight steps password
    | RotatePosition ch -> rotatePosition ch password
    | ReversePositions (pos1, pos2) -> reversePositions pos1 pos2 password
    | MovePosition (pos1, pos2) -> movePosition pos1 pos2 password

let scrambleAll (password : string) operations =
    let rec loop pwd ops =
        match ops with
        | [] -> pwd
        | op :: rest ->
            loop (scramble pwd op) rest
    let scrambled = loop (password |> Seq.toList) operations
    new string(scrambled |> List.toArray)

let readLines =
    File.ReadAllLines
    >> Array.filter (fun line -> line <> String.Empty)
    >> Array.toList

let verify (s : string) (pwd : string) (expected : string) =
    match tryParse s with
    | Some op ->
        let result = scramble (pwd |> Seq.toList) op |> (fun cs -> new string(cs|> List.toArray))
        if result = expected then
            printfn "%s: %s -> %s ✓" s pwd result
        else
            printfn "%s: %s -> %s ✗" s pwd result
    | None -> failwith "?"

let verifySample() =
    printfn "VERIFY SAMPLE"
    printfn "============="
    verify "swap position 4 with position 0" "abcde" "ebcda"
    verify "swap letter d with letter b" "ebcda" "edcba"
    verify "reverse positions 0 through 4" "edcba" "abcde"
    verify "rotate left 1 step" "abcde" "bcdea"
    verify "move position 1 to position 4" "bcdea" "bdeac"
    verify "move position 3 to position 0" "bdeac" "abdec"
    verify "rotate based on position of letter b" "abdec" "ecabd"
    verify "rotate based on position of letter d" "ecabd" "decab"
    printfn "============="

let verifyInput() =
    printfn "VERIFY INPUT"
    printfn "============="
    verify "rotate right 4 steps" "abcdefgh" "efghabcd"
    verify "swap letter b with letter e" "efghabcd" "bfghaecd"
    verify "swap position 1 with position 3" "bfghaecd" "bhgfaecd"
    verify "reverse positions 0 through 4" "bhgfaecd" "afghbecd"
    verify "rotate left 5 steps" "afghbecd" "ecdafghb"
    verify "swap position 6 with position 5" "ecdafghb" "ecdafhgb"
    verify "move position 3 to position 2" "ecdafhgb" "ecadfhgb"
    verify "move position 6 to position 5" "ecadfhgb" "ecadfghb"
    verify "reverse positions 1 through 4" "ecadfghb" "efdacghb"
    verify "rotate right 0 steps" "abcde" "abcde"
    verify "rotate left 0 steps" "abcde" "abcde"
    printfn "============="

let run fileName =
    let lines = readLines fileName
    let operations = lines |> List.choose tryParse
    let password = "abcdefgh"
    let testPwd = "abcde"
    verifySample()
    verifyInput()
    0
    scrambleAll password operations |> printfn "scrambled! %s"
    // password |> printfn "%s"
    // "Not solved." |> printfn "%s"

run "input.txt"
