open System.IO
open System.Text.RegularExpressions

type Degrees =
    | Deg90
    | Deg180
    | Deg270

type Instruction =
    | N of int
    | S of int
    | E of int
    | W of int
    | L of Degrees
    | R of Degrees
    | F of int
    
type Position = (int * int)

type Direction =
    | North
    | West
    | South
    | East
    
type State = (Position * Direction)

let toDegrees x =
    match x with
    | 90 -> Deg90
    | 180 -> Deg180
    | 270 -> Deg270
    | _ -> failwith "Awkward number of degrees %x"

let parseInstruction s =
    let m = Regex.Match(s, "(N|S|E|W|L|R|F)(\d+)")
    let letter = m.Groups.[1].Value
    let x = int m.Groups.[2].Value
    match letter with
    | "N" -> N x
    | "S" -> S x
    | "E" -> E x
    | "W" -> W x
    | "L" -> L <| toDegrees x
    | "R" -> R <| toDegrees x
    | "F" -> F x
    | _ -> failwith <| sprintf "Unknown letter %s" letter
    
let north steps ((x, y), dir) =
    ((x, y+steps), dir)

let west steps ((x, y), dir) =
    ((x-steps, y), dir)

let south steps ((x, y), dir) =
    ((x, y-steps), dir)

let east steps ((x, y), dir) =
    ((x+steps, y), dir)

let forward steps (pos, dir) =
    match dir with
    | North -> north steps (pos, dir)
    | West -> west steps (pos, dir)
    | South -> south steps (pos, dir)
    | East -> east steps (pos, dir)
    
let rotateLeft dir =
    match dir with
    | North -> West
    | West -> South
    | South -> East
    | East -> North

let rotateRight dir =
    match dir with
    | North -> East
    | West -> North
    | South -> West
    | East -> South
    
let rotations deg =
    match deg with
    | Deg90 -> 1
    | Deg180 -> 2 
    | Deg270 -> 3

let rec times n fn =
    if n < 1 then id
    else fn >> times (n - 1) fn 
    
let left deg (pos, dir) =
    let dir' = (times (rotations deg) rotateLeft) dir
    (pos, dir')
    
let right deg (pos, dir) =
    let dir' = (times (rotations deg) rotateRight) dir
    (pos, dir')
    
let move inst =
    match inst with
    | N x -> north x
    | S x -> south x 
    | E x -> east x  
    | W x -> west x  
    | L d -> left d
    | R d -> right d 
    | F x -> forward x

let distance (x, y) : int =
    abs x + abs y

let read =
    Array.toList
    >> List.filter (fun s -> String.length s > 0)
    >> List.map parseInstruction

[<EntryPoint>]
let main argv =
    File.ReadAllLines argv.[0]
    |> read
    |> List.fold (fun state inst -> move inst state) ((0, 0), East)
    |> fst
    |> distance
    |> printfn "%d"
    0 