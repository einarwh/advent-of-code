module Tile exposing (..)

import Array exposing (Array)

type alias Row = Array Char 

type alias Tile = 
  { number : Int
  , lines : Array Row }

create : Int -> Array Row -> Tile 
create number lines = 
    { number = number
    , lines = lines }

sideLength : Tile -> Int
sideLength tile = 
    tile.lines |> Array.get 0 |> Maybe.withDefault Array.empty |> Array.length 

rotateLinesCcw : Array (Array Char) -> Array (Array Char)
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
reverseArray = Array.toList >> List.reverse >> Array.fromList

flipHorizontal : Tile -> Tile 
flipHorizontal tile = 
    { tile | lines = tile.lines |> reverseArray }

flipVertical : Tile -> Tile
flipVertical tile = 
    { tile | lines = tile.lines |> Array.map reverseArray }

north : Tile -> Array Char
north tile = 
    tile.lines |> Array.get 0 |> Maybe.withDefault Array.empty

tryGetLast : Array a -> Maybe a 
tryGetLast arr = 
  let 
    lastIndex = (Array.length arr) - 1
  in 
    arr |> Array.get lastIndex

south : Tile -> Array Char
south tile = 
    tile.lines |> tryGetLast |> Maybe.withDefault Array.empty

west : Tile -> Array Char 
west tile = 
    tile.lines |> Array.map (Array.get 0 >> Maybe.withDefault '.')

east : Tile -> Array Char
east tile = 
    let 
        lastIndex = (tile |> sideLength) - 1
    in 
        tile.lines |> Array.map (Array.get lastIndex >> Maybe.withDefault '.')

sides : Tile -> Array (Array Char)
sides tile = 
    [ north tile
    , west tile
    , south tile
    , east tile ] |> Array.fromList

