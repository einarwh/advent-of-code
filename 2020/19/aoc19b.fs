open System.IO
open System.Text.RegularExpressions

let parseLine (s : string) : (int * string) =
    match s.Split(": ") with
    | [|s1; s2|] -> (int s1, s2)
    | _ -> failwith <| sprintf "malformed line %s" s

let rec parse (ruleNo : int) (resolved : Map<int, string>) (textMap : Map<int, string>)
    : (string * Map<int, string>) =
    match resolved |> Map.tryFind ruleNo with
    | Some s -> (s, resolved)
    | None ->
        let folder (ruleStr, ruleMap) (s : string) : (string * Map<int, string>) =
            let (s, map) = 
                match s with
                | "\"a\"" -> ("a", ruleMap)
                | "\"b\"" -> ("b", ruleMap)
                | "|" -> ("|", ruleMap)
                | _ -> parse (int s) ruleMap textMap
            (ruleStr + s, map)
        let ruleText = textMap |> Map.find ruleNo
        let (ruleStr, ruleMap) =
            ruleText.Split(" ") |> Array.fold folder ("", resolved)
        let rule = if ruleStr.Contains("|") then sprintf "(%s)" ruleStr else ruleStr
        let ruleMap' = ruleMap |> Map.add ruleNo rule
        (rule, ruleMap')

let check (pattern : string) (msg : string)  =
    if Regex.IsMatch(msg, pattern) then Some msg else None    

let read path =
    let parts = File.ReadAllText(path).Replace("\r", "").Split("\n\n")
    (parts.[0].Split("\n"), parts.[1].Split("\n"))

[<EntryPoint>]
let main argv =
    let (ruleLines, messages) = read argv.[0]
    let textMap = ruleLines |> Array.map parseLine |> Map.ofArray
    let map0 = Map.empty
    let (rule42, map1) = parse 42 map0 textMap
    let (rule31, map2) = parse 31 map1 textMap
    let rule8 = sprintf "((%s)+)" rule42
    let rule11 = sprintf "((?<balance>%s)+(?<-balance>%s)+)" rule42 rule31
    let map = map2 |> Map.add 8 rule8 |> Map.add 11 rule11
    let (rule0, _) = parse 0 map textMap
    let pattern = sprintf "^%s$" rule0
    messages |> Array.choose (check pattern) |> Array.length |> printfn "%d"
    0 