// Advent of Code 2020. Day 4, Part B. 
// dotnet fsi aoc04b-maybe.fsx

module Domain =
    
    open System
    open System.Text.RegularExpressions
    
    type BirthYear = private BirthYear of int
    type IssueYear = private IssueYear of int
    type ExpirationYear = private ExpirationYear of int
    type Centimeters = private Centimeters of int
    type Inches = private Inches of int
    type Height = 
        | HeightInCentimeters of Centimeters
        | HeightInInches of Inches
    type HairColor = private HairColor of string
    type EyeColor = private EyeColor of string 
    type PassportId = private PassportId of string
    type CountryId = private CountryId of string
    
    type Passport = {
        byr : BirthYear
        iyr : IssueYear
        eyr : ExpirationYear
        hgt : Height
        hcl : HairColor
        ecl : EyeColor
        pid : PassportId
        cid : CountryId option
    }
    
    let (|Integer|_|) (s : string) =
        match Int32.TryParse s with
        | (true, v) -> Some v
        | _ -> None
        
    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    module BirthYear =
    
        let create (y : int) : BirthYear option =
            if 1920 <= y && y <= 2002 then
                Some (BirthYear y)
            else
                None
    
        let parse (s : string) : BirthYear option =
            match Int32.TryParse s with
            | (true, n) -> create n
            | _ -> None

        let unwrap (birthYear : BirthYear) : int =
            match birthYear with
            | BirthYear y -> y

    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    module IssueYear =
    
        let create (y : int) : IssueYear option =
            if 2010 <= y && y <= 2020 then
                Some (IssueYear y)
            else
                None
    
        let parse (s : string) : IssueYear option =
            match Int32.TryParse s with
            | (true, n) -> create n
            | _ -> None

        let unwrap (year : IssueYear) : int =
            match year with
            | IssueYear y -> y
            
    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    module ExpirationYear =
    
        let create (y : int) : ExpirationYear option =
            if 2020 <= y && y <= 2030 then
                Some (ExpirationYear y)
            else
                None
    
        let parse (s : string) : ExpirationYear option =
            match Int32.TryParse s with
            | (true, n) -> create n
            | _ -> None

        let unwrap (year : ExpirationYear) : int =
            match year with
            | ExpirationYear y -> y
                
    // If cm, the number must be at least 150 and at most 193.
    module Centimeters =
        
        let create (h : int) : Centimeters option =
            if 150 <= h && h <= 193 then
                Some (Centimeters h)
            else
                None
    
        let parse (s : string) : Centimeters option =
            match Int32.TryParse s with
            | (true, n) -> create n
            | _ -> None

        let unwrap (cm : Centimeters) : int =
            match cm with
            | Centimeters n -> n
        
    // If in, the number must be at least 59 and at most 76.
    module Inches =
        
        let create (h : int) : Inches option =
            if 59 <= h && h <= 76 then
                Some (Inches h)
            else
                None
    
        let parse (s : string) : Inches option =
            match Int32.TryParse s with
            | (true, n) -> create n
            | _ -> None

        let unwrap (inches : Inches) : int =
            match inches with
            | Inches n -> n

    // hgt (Height) - a number followed by either cm or in.
    module Height =
        
        let parse (s : string) : Height option =
            let (|Cm|In|Neither|) str =
                if str = "cm" then Cm
                else if str = "in" then In
                else Neither
            let m = Regex.Match(s, "^(\d+)(\w+)$")
            if m.Success then
                match (m.Groups.[1].Value, m.Groups.[2].Value) with
                | (Integer cm, Cm) -> Centimeters.create cm |> Option.map HeightInCentimeters
                | (Integer inches, In) -> Inches.create inches |> Option.map HeightInInches
                | _ -> None
            else
                None
            
        let unwrap (height : Height) : int =
            match height with
            | HeightInCentimeters cm -> Centimeters.unwrap cm
            | HeightInInches inches -> Inches.unwrap inches

    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    module HairColor =
        
        let parse (s : string) : HairColor option =
            if Regex.IsMatch(s, "^#([0-9]|[a-f]){6}$") then
                Some (HairColor s)
            else
                None
                        
        let unwrap (hairColor : HairColor) : string =
            match hairColor with
            | HairColor s -> s

    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    module EyeColor =
        
        let parse (s : string) : EyeColor option =
            if List.contains s ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] then
                Some (EyeColor s)
            else
                None
                        
        let unwrap (eyeColor : EyeColor) : string =
            match eyeColor with
            | EyeColor s -> s

    // pid (Passport ID) - a nine-digit number, including leading zeros.
    module PassportId =
        
        let parse (s : string) : PassportId option =
            if Regex.IsMatch(s, "^\d{9}$") then
                Some (PassportId s)
            else
                None
                        
        let unwrap (pid : PassportId) : string =
            match pid with
            | PassportId s -> s

    module Passport =
        
        open System.Collections.Generic
        
        let private readValues (chunk : string) : IDictionary<string, string> =
            chunk.Trim().Split()
            |> Array.map (fun s -> s.Split(":"))
            |> Array.map (fun a -> a.[0], a.[1])
            |> dict
            
        let private createLookup (d : IDictionary<string, string>) : string -> string option =
            fun key ->
                match d.TryGetValue(key) with
                | (true, s) -> Some s
                | _ -> None
        
        [<Struct>]
        type MaybeBuilder =
            member _.Bind(opt, binder) =
                match opt with
                | Some value -> binder value
                | None -> None
            member _.Return(value) =
                Some value

        let maybe = MaybeBuilder()
        
        let parse (s : string) : Passport option =
            let lookup = readValues s |> createLookup
            maybe {
                let! birthYearStr = lookup "byr"
                let! birthYear = BirthYear.parse birthYearStr
                let! issueYearStr = lookup "iyr"
                let! issueYear = IssueYear.parse issueYearStr
                let! expirationYearStr = lookup "eyr"
                let! expirationYear = ExpirationYear.parse expirationYearStr
                let! heightStr = lookup "hgt"
                let! height = Height.parse heightStr
                let! hairColorStr = lookup "hcl"
                let! hairColor = HairColor.parse hairColorStr
                let! eyeColorStr = lookup "ecl"
                let! eyeColor = EyeColor.parse eyeColorStr
                let! passportIdStr = lookup "pid"
                let! passportId = PassportId.parse passportIdStr
                return {
                    byr = birthYear
                    iyr = issueYear
                    eyr = expirationYear
                    hgt = height
                    hcl = hairColor
                    ecl = eyeColor
                    pid = passportId
                    cid = None
                }
            }

open System.IO
open Domain

let run (text : string) =
    text.Split("\n\n")
    |> Array.map (fun s -> s.Trim())
    |> Array.choose (Passport.parse)
    |> Array.length
    |> printfn "Valid passports: %d"

"input" |> File.ReadAllText |> run 
