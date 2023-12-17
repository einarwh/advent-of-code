module Tile exposing (..)

import Array exposing (Array)

type alias Line = Array Char 

type alias Tile = 
  { number : Int
  , lines : Array Line }

create : Int -> Array Line -> Tile 
create number lines = 
  { number = number
  , lines = lines }

getNumber : Tile -> Int 
getNumber tile = tile.number

getLines : Tile -> Array Line 
getLines tile = tile.lines

getSideLength : Tile -> Int
getSideLength tile = 
  tile.lines |> Array.get 0 |> Maybe.withDefault Array.empty |> Array.length 

rotateLinesCcw : Array Line -> Array Line
rotateLinesCcw lines = 
  let 
    lastIndex = (lines |> Array.get 0 |> Maybe.withDefault Array.empty |> Array.length) - 1
    indexes = List.range 0 lastIndex
  in 
    indexes 
    |> List.map (\i -> lines |> Array.map (\r -> Array.get (lastIndex - i) r |> Maybe.withDefault '.'))
    |> Array.fromList

rotateCcw : Tile -> Tile 
rotateCcw tile = 
  { tile | lines = tile.lines |> rotateLinesCcw }

reverseArray : Array a -> Array a
reverseArray = 
  Array.toList >> List.reverse >> Array.fromList

flipHorizontal : Tile -> Tile 
flipHorizontal tile = 
  { tile | lines = tile.lines |> reverseArray }

flipVertical : Tile -> Tile
flipVertical tile = 
  { tile | lines = tile.lines |> Array.map reverseArray }

north : Tile -> Line
north tile = 
  tile.lines |> Array.get 0 |> Maybe.withDefault Array.empty

tryGetLast : Array a -> Maybe a 
tryGetLast arr = 
  let 
    lastIndex = (Array.length arr) - 1
  in 
    arr |> Array.get lastIndex

south : Tile -> Line
south tile = 
  tile.lines |> tryGetLast |> Maybe.withDefault Array.empty

west : Tile -> Line 
west tile = 
  tile.lines |> Array.map (Array.get 0 >> Maybe.withDefault '.')

east : Tile -> Line
east tile = 
  let 
    lastIndex = (tile |> getSideLength) - 1
  in 
    tile.lines |> Array.map (Array.get lastIndex >> Maybe.withDefault '.')

sides : Tile -> List Line
sides tile = 
  [ north tile
  , west tile
  , south tile
  , east tile ]

sidePermutations : Tile -> List Line 
sidePermutations tile = 
  let 
    s = sides tile 
    r = s |> List.map (reverseArray)
  in 
    s ++ r

rotations : Tile -> List Tile
rotations tile = 
  let 
    n = tile 
    w = n |> rotateCcw
    s = w |> rotateCcw
    e = s |> rotateCcw
    basic = [ n, w, s, e ]
    flipped = basic |> List.map flipVertical
  in 
    basic ++ flipped
