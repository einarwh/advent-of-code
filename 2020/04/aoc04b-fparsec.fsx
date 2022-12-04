// Advent of Code 2020. Day 4, Part B. 
// dotnet fsi aoc04b-fparsec.fsx

#r "nuget: FParsec"

open System
open System.IO
open FParsec

type BirthYear = BirthYear of int
type IssueYear = IssueYear of int
type ExpirationYear = ExpirationYear of int
type Centimeters = Centimeters of int
type Inches = Inches of int
type Height = 
    | HeightInCentimeters of Centimeters
    | HeightInInches of Inches
type HairColor = HairColor of string
type EyeColor = EyeColor of string 
type PassportId = PassportId of string
type CountryId = CountryId of string

type Value =
    | BirthYearValue of BirthYear
    | IssueYearValue of IssueYear
    | ExpirationYearValue of ExpirationYear
    | HeightValue of Height
    | HairColorValue of HairColor
    | EyeColorValue of EyeColor
    | PassportIdValue of PassportId
    | CountryIdValue of CountryId

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

type ParsedPassport =
    | Valid of Passport
    | Invalid of string
    
let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
    let error = messageError msg
    fun stream ->
      let state = stream.State
      let reply = p stream
      if reply.Status <> Ok || predicate reply.Result then reply
      else
          stream.BacktrackTo(state) 
          Reply(Error, error)

let numberInRange (min : int) (max : int) (p: Parser<_,_>) : Parser<_,_> =
    let predicate = (fun n -> min <= n && n <= max)
    let message = sprintf "Number must be in range [%d - %d]." min max
    resultSatisfies predicate message p
    
let space = anyOf " \t\n"

let sepInner = space .>> notFollowedBy space |> attempt 

let sepOuter = space |> many1 

let nonspaceParser = noneOf " \t\n"

let whateverParser = (many1 nonspaceParser) |>> (fun cs -> String.Concat(Array.ofList(cs)))

let byrParser =
    pstring "byr:" >>. (pint32 |> numberInRange 1920 2002) |>> (BirthYear >> BirthYearValue)

let iyrParser =
    pstring "iyr:" >>. (pint32 |> numberInRange 2010 2020) |>> (IssueYear >> IssueYearValue)

let eyrParser =
    pstring "eyr:" >>. (pint32 |> numberInRange 2020 2030) |>> (ExpirationYear >> ExpirationYearValue)

let hgtParser =
    let msg = sprintf "Either a number in range [150-193] followed by 'cm' or a number in the range [59-76] followed by 'in'."
    let cmParser = (pint32 |> numberInRange 150 193) .>> (pstring "cm") |>> (Centimeters >> HeightInCentimeters >> HeightValue)
    let inParser = (pint32 |> numberInRange 59 76) .>> pstring "in" |>> (Inches >> HeightInInches >> HeightValue)
    pstring "hgt:" >>. ((cmParser <|> inParser) <?> msg)

let hclParser =
    pstring "hcl:" >>. regex "^#([0-9]|[a-f]){6}" |>> (HairColor >> HairColorValue)

let eclParser = 
    let eclValueParser = pstring "amb" <|> pstring "blu" <|> pstring "brn" <|> pstring "gry" <|> pstring "grn" <|> pstring "hzl" <|> pstring "oth"
    pstring "ecl:" >>. eclValueParser |>> (EyeColor >> EyeColorValue)

let pidParser =
    pstring "pid:" >>. regex "^\d{9}" |>> (PassportId >> PassportIdValue)

let cidParser =
    pstring "cid:" >>. (many1 (regex "^\S")) |>> (fun cs -> String.concat "" cs |> (CountryId >> CountryIdValue))
 
let propertyParser = byrParser <|> iyrParser <|> eyrParser <|> hgtParser <|> hclParser <|> eclParser <|> pidParser <|> cidParser

let passportPropertiesParser = sepBy1 propertyParser sepInner

let validPassportParser =
    let lookupByr = List.tryPick (function | BirthYearValue y -> Some y | _ -> None)
    let lookupIyr = List.tryPick (function | IssueYearValue y -> Some y | _ -> None)
    let lookupEyr = List.tryPick (function | ExpirationYearValue y -> Some y | _ -> None)
    let lookupHgt = List.tryPick (function | HeightValue h -> Some h | _ -> None)
    let lookupHcl = List.tryPick (function | HairColorValue c -> Some c | _ -> None)
    let lookupEcl = List.tryPick (function | EyeColorValue c -> Some c | _ -> None)
    let lookupPid = List.tryPick (function | PassportIdValue p -> Some p | _ -> None)
    let tryCreatePassport (vs : Value list) : Passport option =
        match (lookupByr vs, lookupIyr vs, lookupEyr vs, lookupHgt vs, lookupHcl vs, lookupEcl vs, lookupPid vs) with
        | (Some byr, Some iyr, Some eyr, Some hgt, Some hcl, Some ecl, Some pid) ->
            Some {
                byr = byr; iyr = iyr; eyr = eyr; hgt = hgt; hcl = hcl; ecl = ecl; pid = pid; cid = None
            }
        | _ -> None
    passportPropertiesParser
    |>> tryCreatePassport
    |> resultSatisfies (fun maybe -> maybe.IsSome) "Missing some required properties"
    |>> (fun maybe -> maybe.Value)
    |>> Valid
    |> attempt

let invalidPassportParser =
    sepBy1 whateverParser sepInner |>> String.concat " " |>> Invalid

let passportParser =
    validPassportParser <|> invalidPassportParser

let passportListParser =
    sepBy1 passportParser sepOuter

let readInput path = path |> File.ReadAllText |> fun s -> s.Trim()

let getValidPassports input =
    match run passportListParser input with
    | Success (parsed, _, _) ->
        parsed |> List.choose (function | Valid p -> Some p | Invalid _ -> None)
    | Failure (str, _, _) -> failwith str 

"input"
|> readInput
|> getValidPassports
|> List.length
|> printfn "Valid passports: %d"
