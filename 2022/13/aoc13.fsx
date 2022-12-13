
open System

type Item = 
    | N of int
    | L of Item list 

let tryRead ix s = 
    if ix < String.length s then 
        Some s.[ix]
    else 
        None

let rec parseItem  ix s = 
    match tryRead ix s with
    | Some '[' -> 
        parseList ix s 
    | Some _ -> 
        parseNumber ix s 
    | None -> failwith "Nothing to read"
and parseNumber ix s = 
    let toNumber acc = 
        acc |> List.rev |> List.toArray |> String |> int |> N
    let rec fn acc ix s = 
        match tryRead ix s with 
        | Some ch when Char.IsDigit ch -> 
            fn (ch :: acc) (ix + 1) s 
        | _ -> 
            (toNumber acc, ix)
    fn [] ix s 
and parseList ix s = 
    let rec fn items ix s = 
        let (item, ix) = parseItem ix s 
        match tryRead ix s with 
        | Some ']' -> 
            (item :: items |> List.rev |> L, ix + 1)
        | Some ',' -> 
            fn  (item :: items) (ix + 1) s
        | _ -> 
            failwith "failed to parse list"
    match tryRead (ix + 1) s with 
    | Some ']' -> (L [], ix + 2)
    | _ -> fn [] (ix + 1) s

let input = "[[[]]]"

input
|> printfn "%A"

input
|> parseItem 0
|> printfn "%A"
