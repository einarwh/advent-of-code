open System.IO

type Exp =
    | Number of int64
    | Add of (Exp * Exp)
    | Mul of (Exp * Exp)

let rec parseValue (tokens : string list) : Exp * string list =
    match tokens with
    | [] -> failwith "ran out of tokens"
    | h :: t ->
        if h = "(" then
            parse t
        else
            (Number (int64 h), t)
and parseNext (prev : Exp) (tokens : string list) : Exp * string list =
    match tokens with
    | [] -> (prev, []) 
    | h :: t ->
        match h with
        | "+" ->
            let (value, after) = parseValue t
            parseNext (Add (prev, value)) after 
        | "*" ->
            let (value, after) = parseValue t
            parseNext (Mul (prev, value)) after 
        | _ -> (prev, t)
and parse (tokens : string list) : Exp * string list =
    let (first, remaining) = parseValue tokens
    if List.isEmpty remaining then
        (first, [])
    else 
        parseNext first remaining

let parseExp tokens =
    parse tokens |> fst

let rec evaluate exp =
    match exp with
    | Number n -> n
    | Add (x, y) -> evaluate x + evaluate y
    | Mul (x, y) -> evaluate x * evaluate y
    
let tokenize (s : string) =
    let rec toString chars = new string(List.toArray chars)
    let rec tk (acc : char list) (remaining : char list) : string list =
        match remaining with
        | [] ->
            if List.isEmpty acc then [] else [ toString acc ]
        | ch :: rest ->
            if ch = ' ' then
                if List.isEmpty acc then
                    tk [] rest
                else
                    (toString acc) :: (tk [] rest)
            elif ch = '(' then
                "(" :: (tk [] rest)
            elif ch = ')' then
                if List.isEmpty acc then
                     ")" :: (tk [] rest)
                else 
                    toString acc :: ")" :: (tk [] rest)
            else
                tk (acc @ [ch]) rest
    s |> Seq.toList |> tk []
    
let calculate = tokenize >> parseExp >> evaluate
   
let run fileName =
    let lines =
        File.ReadAllLines fileName
        |> Array.toList
        |> List.filter (fun s -> s.Length > 0)
    lines
    |> List.map calculate
    |> List.sum
    |> printfn "%d"
    0 

"input.txt" |> run