// Advent of Code 2017. Day 14: Disk Defragmentation.
// This takes a while to execute.
// dotnet fsi aoc14.fsx

open System
open System.IO

let rev n list = 
  let rec help n fst list = 
    if n = 0 then fst @ list
    else
      match list with 
      | [] -> failwith "not enough elements"
      | h :: t -> help (n - 1) (h :: fst) t
  help n [] list  

let rec times n fn = 
  if n > 0 then fn >> times (n - 1) fn else id
  
let rot = function
  | [] -> []
  | h::t -> t @ [h]  
  
let rotl n = times n rot

let rotr n lst = times (List.length lst - n) rot lst

let step (pos, skip, list) length = 
  let list' = list |> rotl pos |> rev length |> rotr pos
  let pos' = (pos + length + skip) % List.length list 
  let skip' = skip + 1
  (pos', skip', list')

let execute list = List.fold step (0, 0, list)

let rec copies n list = 
  if n = 1 then list 
  else list @ copies (n - 1) list

let rec blocks = function  
  | [] -> []
  | list -> 
    let block = Seq.take 16 list |> Seq.toList
    let rest = Seq.skip 16 list |> Seq.toList
    block :: blocks rest 

let densify = 
  blocks
  >> List.map (List.reduce (^^^) >> sprintf "%x")
  >> List.map (fun s -> s.PadLeft(2, '0'))
  >> String.concat ""

let hash input = 
  let input' = (input |> Seq.map int |> Seq.toList) @ [17;31;73;47;23]
  let (_, _, sparse) = execute [ 0 .. 255 ] (copies 64 input')
  let result = densify sparse 
  result

let bitcount = function 
  | '0' -> 0
  | '1' -> 1
  | '2' -> 1
  | '3' -> 2
  | '4' -> 1
  | '5' -> 2
  | '6' -> 2
  | '7' -> 3
  | '8' -> 1
  | '9' -> 2
  | 'a' -> 2
  | 'b' -> 3
  | 'c' -> 2
  | 'd' -> 3
  | 'e' -> 3
  | 'f' -> 4
  | _ -> failwith "?"

let bits = function 
  | '0' -> [0;0;0;0]
  | '1' -> [0;0;0;1]
  | '2' -> [0;0;1;0]
  | '3' -> [0;0;1;1]
  | '4' -> [0;1;0;0]
  | '5' -> [0;1;0;1]
  | '6' -> [0;1;1;0]
  | '7' -> [0;1;1;1]
  | '8' -> [1;0;0;0]
  | '9' -> [1;0;0;1]
  | 'a' -> [1;0;1;0]
  | 'b' -> [1;0;1;1]
  | 'c' -> [1;1;0;0]
  | 'd' -> [1;1;0;1]
  | 'e' -> [1;1;1;0]
  | 'f' -> [1;1;1;1]
  | _ -> failwith "?"

let solve input =
    [0 .. 127] 
    |> Seq.collect (sprintf "%s-%d" input >> hash) 
    |> Seq.sumBy bitcount

let contains n = List.exists (fun m -> n = m)
  
let rec group lookup members key = 
  if contains key members then members
  else  
    let members' = key :: members
    lookup key 
    |> List.filter (not << fun t -> contains t members')
    |> List.fold (group lookup) members'
  
let makeLookup vals = 
  let map = Map.ofList vals
  fun it -> Map.find it map 

let numberList = [0 .. 127]
let rowcount = List.length numberList
let hashlength = 128

let makeExists list = 
  let map = Map.ofList list
  fun (r, i) ->
    if r >= 0 && r < rowcount && i >= 0 && i < hashlength then   
      let row = Map.find r map 
      List.exists (fun x -> i = x) row
    else false

let number (r, i) = 
  r * hashlength + i

let neighbours exists (rn, x) = 
    [ (rn - 1, x)
      (rn, x - 1)
      (rn, x + 1) 
      (rn + 1, x) ]
    |> List.choose (fun pair -> if exists pair then Some pair else None)

let getPairs input = 
    let list = 
      numberList
      |> Seq.map (sprintf "%s-%d" input >> hash >> Seq.collect bits >> Seq.toList)
      |> Seq.mapi (fun i bs -> i, bs |> List.mapi (fun j b -> if b = 0 then None else Some <| j) |> List.choose id)
      |> Seq.toList
    let exists = makeExists list
    list 
    |> List.collect (fun (rowno, row) -> row |> List.map (fun x -> (rowno, x)))
    |> List.map (fun (rn, x) -> number (rn, x), neighbours exists (rn, x) |> List.map number)
    
let groups vals = 
  let lookup = makeLookup vals
  let rec solve all gs = function 
  | [] -> gs
  | (n, _) :: t ->
    if contains n all then 
      solve all gs t
    else     
      let g = group lookup [] n
      solve (g @ all) (g :: gs) t
  solve [] [] vals |> List.length

let solve2 = getPairs >> groups

let readText fileName = 
    File.ReadAllText(fileName).Trim()

let run fileName = 
    let text = readText fileName
    text |> solve |> printfn "%A"
    text |> solve2 |> printfn "%A"

run "input.txt"
