// Advent of Code 2020. Day 4, Part B. 
// dotnet fsi aoc04b.fsx

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let (|Integer|_|) (s : string) =
    match Int32.TryParse s with
    | (true, v) -> Some v
    | _ -> None
    
let readValues (chunk : string) : IDictionary<string, string> =
    chunk.Trim().Split()
    |> Array.map (fun s -> s.Split(":"))
    |> Array.map (fun a -> a.[0], a.[1])
    |> dict
        
let makeCheck (key : string) (validator : string -> bool) (values : IDictionary<string, string>) : bool =
    match values.TryGetValue(key) with
    | (true, s) -> validator s
    | (false, _) -> false        

let inYearRange (min : int) (max : int) (s : string) : bool =
    match s with
    | Integer n -> min <= n && n <= max
    | _ -> false

// byr (Birth Year) - four digits; at least 1920 and at most 2002.
let validBirthYear = inYearRange 1920 2002

let checkBirthYear = makeCheck "byr" validBirthYear

// iyr (Issue Year) - four digits; at least 2010 and at most 2020.
let validIssueYear = inYearRange 2010 2020
    
let checkIssueYear = makeCheck "iyr" validIssueYear

// eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
let validExpirationYear = inYearRange 2020 2030

let checkExpirationYear = makeCheck "eyr" validExpirationYear

// hgt (Height) - a number followed by either cm or in:
//    If cm, the number must be at least 150 and at most 193.
//    If in, the number must be at least 59 and at most 76.
let validHeight (s : string) : bool =
    let (|Cm|In|Neither|) str =
        if str = "cm" then Cm
        else if str = "in" then In
        else Neither
    let m = Regex.Match(s, "^(\d+)(\w+)$")
    if m.Success then
        match (m.Groups.[1].Value, m.Groups.[2].Value) with
        | (Integer cm, Cm) -> 150 <= cm && cm <= 193 
        | (Integer inches, In) -> 59 <= inches && inches <= 76
        | _ -> false
    else
        false

let checkHeight = makeCheck "hgt" validHeight

// hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
let validHairColor (s : string) : bool =
    Regex.IsMatch(s, "^#([0-9]|[a-f]){6}$")

let checkHairColor = makeCheck "hcl" validHairColor

// ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
let validEyeColor (s : string) : bool =
    List.contains s ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    
let checkEyeColor = makeCheck "ecl" validEyeColor

// pid (Passport ID) - a nine-digit number, including leading zeros.
let validPassportId (s : string) : bool =
    Regex.IsMatch(s, "^\d{9}$")

let checkPassportId = makeCheck "pid" validPassportId
    
let check (values : IDictionary<string, string>) : bool =
    [ checkBirthYear
      checkIssueYear
      checkExpirationYear
      checkHeight
      checkHairColor
      checkEyeColor
      checkPassportId ]
    |> List.map (fun c -> c values)
    |> List.reduce (&&)

let run (text : string) =
    text.Split("\n\n")
    |> Array.map readValues
    |> Array.filter check
    |> Array.length
    |> printfn "Valid passports: %d"

"input" |> File.ReadAllText |> run 