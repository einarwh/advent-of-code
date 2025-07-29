module Main exposing (..)

-- Advent of Code 2022. 
-- Day 9: Rope Bridge.

import Browser 
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultDelay : Float
defaultDelay = 200

crateSize : Int 
crateSize = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type Direction = U | L | D | R

type alias Motion = (Direction, Int)

type alias Pos = (Int, Int)

type alias Rope = (Pos, List Pos) 

type alias Model = 
  { rope : Rope 
  , directions : List Direction
  , visited : Set Pos 
  , delay : Float
  , paused : Bool
  , follow : Bool 
  , moves : Int 
  , debug : String }

tryParseDir : String -> Maybe Direction
tryParseDir s = 
  case s of 
    "U" -> Just U
    "R" -> Just R
    "D" -> Just D
    "L" -> Just L
    _ ->  Nothing

tryParseMotion : String -> Maybe Motion 
tryParseMotion s = 
  case s |> String.split " " of 
    dirStr :: stepsStr :: _ -> 
      let 
        maybeDir = dirStr |> tryParseDir 
        steps = stepsStr |> String.toInt |> Maybe.withDefault 0 
      in 
        maybeDir |> Maybe.map (\dir -> (dir, steps))
    _ -> Nothing 
  
toDirections : Motion -> List Direction 
toDirections (dir, steps) = 
  List.range 1 steps |> List.map (\_ -> dir)

toDirectionText : Direction -> String 
toDirectionText dir = 
  case dir of 
    U -> "UP"
    R -> "RIGHT"
    D -> "DOWN"
    L -> "LEFT"

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

    input = """L 2
D 2
U 1
R 1
U 2
D 1
L 1
D 1
R 2
D 2
U 1
R 2
U 2
R 2
U 2
L 2
U 2
L 2
U 1
D 1
L 1
R 1
U 2
R 2
D 1
U 2
L 1
U 2
L 2
D 2
R 1
U 2
R 2
U 1
D 1
L 1
U 2
L 2
D 1
U 1
R 2
D 2
R 1
D 1
L 1
R 2
U 1
L 1
D 2
U 2
D 1
L 1
R 1
U 1
D 1
R 2
D 2
R 1
D 2
L 1
D 2
L 1
U 2
R 1
L 1
U 2
L 2
D 2
R 2
L 1
U 2
R 1
U 2
R 2
L 2
D 2
R 1
L 1
D 1
L 1
U 2
L 2
D 2
L 2
U 1
R 2
U 1
R 2
D 2
U 1
R 1
U 2
R 2
L 1
U 2
L 1
U 2
L 1
U 1
R 2
U 2
L 1
D 2
U 1
L 2
U 2
D 2
U 1
D 1
R 1
D 2
U 2
R 2
L 2
R 2
U 1
L 2
R 3
D 2
L 1
R 1
D 3
U 3
R 2
L 3
U 1
L 3
U 2
L 2
U 3
D 2
R 1
U 1
R 3
U 2
D 2
U 3
D 3
L 2
U 2
L 3
U 1
R 1
D 3
L 1
R 2
U 1
L 2
D 1
U 2
L 2
U 1
D 1
U 1
R 3
U 3
R 2
U 1
D 1
U 2
D 1
R 1
U 3
R 1
U 3
R 1
U 2
D 1
L 1
D 1
L 1
D 1
U 1
L 2
R 2
U 2
L 3
U 1
D 1
R 3
U 1
L 2
R 1
D 1
R 3
D 2
U 2
D 1
R 1
U 1
D 1
U 1
D 3
U 1
L 3
D 1
R 2
U 2
R 2
U 1
R 2
D 2
L 3
U 1
D 1
L 3
U 3
R 1
U 3
D 3
L 3
D 3
L 2
R 2
U 2
R 1
U 2
D 2
U 2
L 2
R 1
U 3
R 4
L 2
R 4
D 2
R 2
L 2
R 2
D 1
U 2
D 3
R 1
U 1
D 4
R 4
L 1
U 1
D 2
L 2
R 3
U 2
L 3
R 4
U 3
L 3
D 4
R 3
L 2
U 3
D 1
L 2
U 3
L 1
D 2
R 1
D 4
L 2
R 4
U 4
L 2
R 2
D 4
L 3
U 3
L 2
U 2
L 4
D 2
R 4
D 1
L 4
U 4
R 1
U 4
D 2
R 2
U 3
L 1
R 4
U 3
R 3
U 4
L 2
D 3
L 4
R 2
L 4
D 4
U 1
L 2
D 4
R 1
U 3
R 1
U 4
L 1
D 4
U 2
L 4
U 1
R 2
U 4
L 2
D 3
L 2
U 4
D 2
L 3
D 1
U 4
D 1
L 4
U 3
R 3
L 2
R 2
U 2
D 2
L 1
U 4
R 2
L 3
R 1
U 4
L 2
U 4
L 2
U 3
L 2
R 3
U 3
L 2
D 4
R 3
U 2
L 3
D 3
R 4
U 1
R 3
D 5
U 1
L 1
R 5
D 2
L 3
U 5
D 3
L 3
D 1
U 3
R 2
U 5
L 2
D 2
L 5
U 2
D 5
R 1
U 1
R 5
L 4
D 4
R 2
U 5
D 5
L 1
R 2
U 4
L 2
D 1
L 5
D 1
R 3
L 2
R 4
D 1
R 4
U 4
L 1
U 2
D 5
U 4
L 3
D 3
R 3
D 5
L 2
U 5
R 3
L 5
R 4
U 4
R 3
L 2
D 4
L 2
U 1
R 3
L 2
R 3
L 5
U 1
L 2
R 1
D 3
R 4
L 2
D 2
R 4
D 3
L 5
D 2
L 4
R 3
L 3
D 2
R 5
U 4
R 4
U 3
L 4
U 1
R 4
D 4
U 1
L 3
U 1
L 1
R 4
D 1
U 3
D 2
R 3
L 3
U 2
R 4
D 4
L 1
R 5
U 1
L 2
U 2
R 4
U 4
R 2
D 2
U 3
L 5
D 4
L 5
D 1
L 2
D 4
R 6
L 3
D 5
R 3
L 5
R 5
D 1
R 3
L 5
R 5
L 6
R 4
L 2
R 5
D 3
U 1
R 4
D 2
R 2
L 6
U 2
R 5
U 5
L 3
R 6
L 6
D 3
L 1
R 1
L 1
D 5
L 4
U 3
D 5
L 1
U 6
R 5
D 6
U 1
D 5
L 3
D 5
R 6
U 2
D 1
L 4
D 5
R 2
U 5
R 4
L 1
R 2
L 1
R 2
L 6
U 2
D 3
R 5
D 3
L 4
R 4
D 1
R 6
D 1
R 4
L 3
D 6
L 5
D 2
R 1
L 3
R 2
L 6
D 4
U 5
R 6
U 3
D 2
R 4
L 6
U 4
L 2
U 2
L 1
U 5
D 1
L 2
U 1
D 2
L 1
R 4
U 4
L 4
D 4
R 3
D 6
L 2
U 4
R 1
L 2
U 3
L 6
U 6
R 3
D 7
U 4
R 6
L 6
D 7
L 5
U 1
R 2
U 7
D 5
L 7
R 2
U 3
R 7
D 5
L 2
U 2
R 3
U 5
R 3
D 4
R 4
U 3
L 6
R 2
D 7
U 6
D 1
U 3
R 3
D 4
R 6
U 4
L 3
D 3
R 4
L 2
D 1
R 3
U 6
R 5
U 3
L 5
R 5
D 4
U 3
L 7
U 7
D 3
U 1
D 1
U 1
D 3
L 5
R 4
U 6
R 7
D 2
L 3
R 2
U 3
D 2
L 5
D 7
R 6
D 1
U 5
D 7
U 6
D 4
L 6
D 4
L 2
D 7
R 6
L 3
D 2
L 3
D 2
U 6
L 1
R 2
U 5
L 5
R 2
U 6
R 3
L 7
R 4
D 7
L 7
D 6
L 4
D 2
R 6
L 1
D 3
U 6
D 7
U 1
L 5
D 7
U 3
R 3
L 7
D 2
L 7
U 5
D 4
R 3
D 8
R 7
U 1
R 8
U 4
D 5
R 6
L 5
D 5
L 7
R 7
U 4
D 2
L 7
U 8
D 6
L 5
D 2
L 6
U 6
R 5
U 5
L 6
U 5
L 3
U 4
D 7
R 3
L 4
R 7
D 1
R 5
D 2
R 8
L 8
U 8
L 6
U 5
L 1
U 6
R 2
U 4
L 5
R 2
D 8
L 1
U 4
R 6
U 1
L 4
R 7
L 8
R 5
U 5
R 3
L 3
U 2
D 8
L 8
R 4
U 3
D 3
L 1
R 8
U 3
L 8
D 8
U 7
D 8
L 3
U 5
D 7
L 7
U 5
L 3
R 6
D 7
R 1
D 7
L 5
U 3
D 5
U 1
L 2
D 4
U 5
R 1
L 6
U 3
L 1
D 6
R 1
L 1
R 3
L 4
U 6
R 3
U 8
L 2
U 3
R 1
D 5
R 2
U 2
D 8
L 3
D 2
R 8
D 1
L 3
U 1
D 8
U 1
L 2
D 4
R 3
L 4
D 3
L 2
D 4
L 6
U 6
D 9
U 5
R 9
D 1
R 8
L 9
R 7
D 5
U 1
L 1
R 8
U 3
R 4
L 2
R 1
D 6
U 5
D 6
L 9
R 2
D 2
R 8
D 2
R 3
U 1
L 1
U 1
L 1
U 3
D 9
R 2
U 1
D 1
R 6
D 7
L 9
U 4
L 7
U 8
D 8
L 3
D 9
L 4
D 8
U 9
L 9
D 7
U 7
D 2
L 4
D 5
L 5
U 6
D 8
L 5
U 9
L 4
U 3
D 4
R 9
U 4
D 7
L 6
R 7
L 5
U 5
L 8
D 3
L 5
U 7
D 6
U 3
R 1
D 8
U 3
L 8
U 9
R 1
U 1
L 3
R 6
U 5
R 7
U 4
R 4
L 8
U 4
L 9
R 7
D 6
R 8
U 9
D 8
L 3
R 7
U 1
L 9
R 2
L 3
D 6
R 9
D 1
L 3
D 4
U 2
D 9
R 9
U 3
R 2
D 3
U 7
L 4
D 6
R 3
D 8
U 4
R 8
U 6
R 5
L 5
U 2
D 1
R 5
U 9
D 3
L 5
R 8
U 2
D 9
R 5
U 4
L 7
U 4
D 6
U 7
D 1
L 6
U 7
D 6
L 3
D 8
U 1
L 1
U 9
L 2
U 4
D 2
R 8
U 3
R 8
L 4
U 3
L 7
U 7
D 7
R 7
L 2
U 6
R 1
U 7
D 6
L 1
R 5
L 7
R 1
D 7
U 9
D 9
L 1
D 10
U 10
L 2
R 9
L 1
R 3
U 8
L 1
R 2
L 2
U 2
D 1
U 5
R 5
D 7
L 6
D 2
L 10
U 10
D 4
L 10
R 10
U 5
L 7
R 10
D 1
L 8
D 3
U 7
R 2
D 7
U 4
L 3
R 4
L 10
U 3
L 5
R 4
D 3
R 9
L 9
R 6
U 5
L 5
R 11
U 6
L 11
R 9
D 5
U 7
L 8
R 5
U 11
D 6
L 9
D 5
L 2
D 2
R 4
L 1
D 10
L 9
R 6
D 2
L 11
D 4
R 5
D 2
R 8
D 3
L 1
R 11
D 1
L 11
D 7
L 5
U 7
D 4
L 7
U 11
L 3
U 7
L 2
D 6
R 1
D 3
R 1
L 1
D 11
L 6
U 1
R 9
U 8
D 8
L 2
R 6
U 10
R 7
L 5
U 1
R 4
L 2
D 11
L 10
U 7
L 10
D 1
R 6
U 2
D 7
R 7
D 2
R 10
U 8
D 5
U 2
L 11
R 9
D 7
U 3
R 6
U 6
D 7
U 6
L 2
R 11
L 10
D 7
U 10
D 3
R 10
L 4
D 8
R 5
L 5
D 9
U 8
R 3
D 10
R 11
U 5
D 11
L 2
D 8
L 7
U 4
D 5
U 9
D 3
U 11
D 4
L 1
U 4
L 3
R 7
L 12
D 10
U 8
L 6
D 11
U 5
L 2
D 6
L 5
U 8
L 4
U 6
R 9
D 2
L 6
U 12
R 2
L 10
R 7
U 7
L 5
R 6
U 5
R 7
D 6
R 4
U 6
L 4
U 4
L 4
U 3
D 3
L 9
D 12
R 10
U 3
L 1
U 4
L 8
R 4
D 6
L 2
D 7
L 9
U 3
L 11
R 9
D 8
L 1
D 11
R 8
L 6
U 7
R 5
U 3
D 4
R 4
U 5
L 6
U 10
D 2
R 8
U 7
D 4
L 11
R 9
U 9
D 12
U 8
R 4
U 5
D 11
L 5
R 5
D 2
L 12
U 4
L 1
R 9
L 5
D 1
R 2
L 2
D 7
L 12
D 2
R 7
U 10
L 1
U 2
L 8
D 5
L 7
U 8
L 8
U 10
D 7
R 8
D 7
R 9
L 7
D 1
R 3
D 4
R 11
D 3
L 5
D 7
U 8
L 2
D 10
L 12
R 7
U 13
R 4
U 10
L 10
D 6
U 7
D 12
U 2
D 8
L 1
R 5
D 9
L 1
R 11
L 2
D 7
L 13
U 1
L 8
D 2
U 12
L 6
R 10
L 10
R 9
U 3
R 1
L 10
D 2
L 4
D 8
U 7
L 11
D 8
U 1
D 1
L 2
D 2
L 5
U 6
D 1
U 6
R 2
L 2
D 5
U 2
R 10
L 4
D 3
L 11
D 11
U 4
D 4
R 12
D 11
U 2
R 11
U 3
R 8
U 8
L 5
R 12
U 1
L 10
R 11
L 3
U 6
D 2
R 7
D 13
L 11
R 8
L 8
R 1
L 5
D 13
R 8
L 12
D 13
R 10
U 5
D 13
U 6
L 2
D 2
U 7
D 4
L 4
R 5
L 12
U 6
D 8
U 7
D 12
R 11
L 11
D 9
R 4
D 3
L 5
R 6
D 9
R 3
U 10
R 10
U 3
D 11
L 3
D 5
R 14
U 7
R 3
D 1
L 4
U 6
D 6
R 10
L 12
R 10
L 4
U 10
R 8
D 14
L 6
U 7
D 4
L 9
R 10
D 10
L 4
R 6
U 8
D 10
L 12
U 11
L 7
R 1
U 1
R 12
U 7
D 3
U 4
R 3
D 6
U 2
R 2
U 14
D 10
R 5
L 12
U 5
D 6
R 7
L 13
D 11
R 6
D 11
R 6
D 6
U 10
L 14
D 4
R 14
D 7
U 12
L 3
U 2
D 3
R 1
D 3
U 2
D 13
L 14
R 13
U 3
D 12
L 14
U 12
D 6
R 2
U 6
L 11
D 3
L 7
D 7
R 11
D 11
R 10
L 2
U 12
L 14
U 11
R 12
L 4
D 2
R 13
L 1
U 7
L 2
D 7
L 14
R 11
U 1
L 7
U 11
L 6
U 1
D 11
L 14
R 4
L 1
D 14
U 7
L 6
R 3
L 2
U 10
D 3
R 4
U 7
R 11
U 7
R 13
U 2
L 12
R 9
L 15
R 4
U 5
D 7
L 10
R 9
D 5
L 14
U 9
R 3
L 15
R 5
L 10
D 5
U 3
D 9
U 13
D 15
R 10
L 9
R 9
U 4
D 2
L 3
R 5
L 11
R 7
L 15
D 12
L 13
D 4
R 2
U 14
D 9
L 1
D 1
L 6
U 9
D 14
U 2
L 1
R 10
U 14
R 4
D 11
R 6
D 11
U 9
R 2
L 6
R 10
L 4
D 7
R 11
U 5
L 2
D 5
U 6
R 8
U 4
R 9
D 15
R 8
L 10
U 5
L 4
U 7
L 3
U 5
R 2
U 13
L 9
U 4
D 13
R 1
D 15
U 4
L 7
D 12
L 8
U 6
L 10
U 13
R 12
D 13
U 6
D 13
U 2
L 13
U 7
L 8
U 6
D 2
U 13
L 2
D 13
R 3
D 2
L 11
D 13
U 1
D 12
L 9
U 2
R 9
U 16
D 6
L 16
D 15
R 7
U 3
D 11
L 8
U 3
L 15
R 7
D 14
L 8
R 7
U 12
R 11
U 2
R 13
D 9
L 4
U 14
L 6
D 7
L 10
U 14
D 8
L 6
D 3
L 6
D 12
U 13
R 9
L 6
U 9
R 9
D 13
R 13
U 13
L 4
D 1
R 8
L 1
R 15
U 15
R 14
D 9
L 5
D 2
L 14
U 14
L 15
R 10
U 11
L 7
R 12
U 7
L 7
D 9
U 1
L 11
R 11
D 13
U 7
R 7
U 7
R 8
D 12
U 11
D 14
U 16
L 5
R 8
D 2
L 10
D 14
R 16
D 13
U 14
L 6
R 11
D 4
R 15
D 2
R 3
L 2
U 8
L 4
D 5
U 3
L 11
U 7
R 14
U 4
L 5
D 10
U 16
L 1
U 5
R 11
D 16
U 5
D 11
R 5
D 9
R 3
U 13
D 6
L 10
D 2
U 4
D 7
U 1
R 10
D 6
U 12
L 2
D 8
U 5
L 5
U 7
L 3
U 16
D 12
L 12
R 16
L 5
R 17
L 7
U 17
D 15
R 14
L 7
U 16
R 7
D 7
R 14
D 6
L 7
U 6
R 5
D 14
L 8
R 3
L 1
U 12
L 8
D 11
R 3
D 11
L 16
R 4
U 2
R 17
U 10
R 3
U 13
R 6
D 16
U 15
R 3
U 7
D 6
U 2
L 12
D 11
L 6
D 9
R 7
U 10
R 14
D 10
R 10
D 17
R 9
L 11
R 7
D 14
L 10
U 17
R 5
D 12
U 11
D 11
R 7
D 12
U 10
D 10
U 8
D 11
R 8
U 9
L 17
D 15
R 2
D 14
L 10
U 2
L 9
R 15
L 8
D 9
L 7
R 10
D 6
L 15
U 1
L 11
R 14
L 7
R 17
U 17
L 8
U 16
D 4
R 9
L 13
R 5
L 1
D 14
U 7
L 8
R 10
U 10
D 16
U 6
R 13
D 12
U 10
L 3
U 2
R 7
D 12
R 16
D 1
L 14
R 9
D 18
R 1
L 17
R 9
U 16
L 5
U 12
R 7
D 6
L 17
U 3
R 5
U 13
D 9
U 12
L 13
D 5
R 1
U 7
L 4
R 13
D 1
L 15
U 11
L 5
D 2
R 12
D 12
U 5
L 1
R 5
U 3
R 5
L 9
R 15
U 3
D 18
L 10
R 2
D 7
R 13
L 8
R 5
L 2
R 15
D 12
L 14
U 15
L 8
U 13
R 13
L 15
R 16
D 10
L 15
U 1
R 1
D 3
U 17
D 2
L 5
R 17
U 6
R 10
L 3
D 11
R 13
D 5
U 3
D 9
U 10
R 12
D 10
R 15
U 11
L 6
R 3
U 4
D 7
R 12
L 5
R 7
D 17
L 14
U 9
D 7
U 2
R 1
D 13
R 3
D 12
L 3
U 17
D 1
U 6
L 8
U 17
D 1
R 6
L 8
R 8
U 5
L 12
D 8
R 5
L 11
U 17
L 3
U 11
L 1
R 15
D 15
L 10
D 10
L 19
R 11
L 4
D 11
L 13
U 4
D 6
L 3
U 11
R 3
L 9
D 4
U 10
L 14
D 10
R 11
L 1
R 1
D 8
R 19
D 6
L 16
D 1
U 13
R 19
U 19
D 3
R 8
D 16
R 2
D 2
L 19
D 7
R 2
U 4
L 5
R 2
U 9
L 5
D 13
L 5
R 14
L 13
D 5
U 10
R 12
L 11
D 18
U 7
L 12
R 11
D 16
U 15
L 12
R 12
D 1
R 2
D 6
L 15
D 8
R 19
U 9
R 12
L 10
R 3
D 19
R 15
U 12
R 6
L 3
D 17
R 2
U 11
D 12
L 15
D 16
R 18
D 16
U 12
R 16
U 17
R 15
L 10
R 4
D 12
L 5
R 11
L 1
U 11
R 3
D 5"""

    motions = input |> String.split "\n" |> List.filterMap tryParseMotion 
    directions = motions |> List.concatMap toDirections

    origin = (0, 0)
    head = origin
    tail = List.range 1 9 |> List.map (\_ -> origin)

    model = { rope = (head, tail)
            , directions = directions
            , visited = Set.empty 
            , delay = defaultDelay
            , paused = True
            , follow = True
            , moves = 0  
            , debug = ".." }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick | Step | TogglePlay | Faster | Slower | Longer | Shorter | ToggleZoom

updateHead : Direction -> Pos -> Pos 
updateHead d (x, y) = 
  case d of 
    U -> (x, y+1)
    L -> (x-1, y)
    D -> (x, y-1)
    R -> (x+1, y)

knotMoveDict = Dict.fromList
    [ ((-2,  2), (-1,  1))
    , ((-1,  2), (-1,  1))
    , (( 0,  2), ( 0,  1))
    , (( 1,  2), ( 1,  1))
    , (( 2,  2), ( 1,  1))
    , (( 2,  1), ( 1,  1))
    , (( 2,  0), ( 1,  0))
    , (( 2, -1), ( 1, -1))
    , (( 2, -2), ( 1, -1))
    , (( 1, -2), ( 1, -1))
    , (( 0, -2), ( 0, -1))
    , ((-1, -2), (-1, -1))
    , ((-2, -2), (-1, -1))
    , ((-2, -1), (-1, -1))
    , ((-2,  0), (-1,  0))
    , ((-2,  1), (-1,  1)) ]

toKnotMove : Pos -> Pos -> Pos
toKnotMove (hx, hy) (tx, ty) = 
  let 
    key = (hx - tx, hy - ty)
  in 
   knotMoveDict |> Dict.get key |> Maybe.withDefault (0, 0)

updateKnot : Pos -> Pos -> Pos 
updateKnot (hx, hy) (tx, ty) = 
  let 
    (dx, dy) = toKnotMove (hx, hy) (tx, ty)
  in 
    (tx + dx, ty + dy)

updateTail : Pos -> List Pos -> List Pos 
updateTail prev tail = 
  case tail of 
    [] -> [] 
    knot :: rest -> 
      let 
        updatedKnot = updateKnot prev knot 
      in 
        updatedKnot :: (updateTail updatedKnot rest)

updateRope : Direction -> Rope -> Rope 
updateRope dir (head, tail) = 
  let 
    updatedHead = head |> updateHead dir 
    updatedTail = tail |> updateTail updatedHead
  in 
    (updatedHead, updatedTail)

updateModel : Model -> Model
updateModel model = 
  case model.directions of 
    [] -> model 
    d :: remainingDirections -> 
      let 
        updatedRope = updateRope d model.rope 
        (updatedHead, updatedTail) = updatedRope 
        updatedMoves = model.moves + 1
        lastDirectionText = toDirectionText d
        lastKnot = updatedTail |> List.reverse |> List.head |> Maybe.withDefault updatedHead
        updatedVisited = model.visited |> Set.insert lastKnot
      in 
        { model | rope = updatedRope
        , directions = remainingDirections
        , visited = updatedVisited
        , moves = updatedMoves
        , debug = lastDirectionText  }

shrinkRope : Rope -> Rope  
shrinkRope (head, tail) = 
  let 
    updatedTail = 
      case List.reverse tail of 
        [] -> []
        _ :: t -> 
          List.reverse t
  in 
    (head, updatedTail)

growRope : Rope -> Rope 
growRope (head, tail) = 
  if List.length tail >= 19 then
    (head, tail) 
  else  
    let 
      updatedTail = 
        case List.reverse tail of 
          [] -> [ head ]
          h :: t -> List.reverse (h :: h :: t) 
    in 
      (head, updatedTail)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)
    Step ->
      (updateModel model, Cmd.none)
    TogglePlay -> 
      ({model | paused = not model.paused }, Cmd.none)
    Faster -> 
      ({model | delay = model.delay / 2 }, Cmd.none)
    Slower -> 
      ({model | delay = model.delay * 2 }, Cmd.none)
    Longer -> 
      ({model | rope = growRope model.rope }, Cmd.none) 
    Shorter -> 
      ({model | rope = shrinkRope model.rope }, Cmd.none) 
    ToggleZoom -> 
      ({model | follow = not model.follow } , Cmd.none) 

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused || List.isEmpty model.directions then Sub.none 
  else Time.every model.delay (\_ -> Tick)

-- VIEW

toKnotCircle : String -> Pos -> Html Msg 
toKnotCircle color (x, y) = 
  let 
    xVal = x
    yVal = -y 
  in
    circle
      [ cx (String.fromInt xVal)
      , cy (String.fromInt yVal)
      , r (String.fromFloat 0.5)
      , fill color ]
      []

toHeadCircle : Pos -> Html Msg 
toHeadCircle (x, y) = toKnotCircle "#50C878" (x, y)

toTailCircle : Pos -> Html Msg 
toTailCircle (x, y) = toKnotCircle "#5F8575" (x, y)

toVisitedRect : Pos -> Html Msg 
toVisitedRect (x1, y1) = 
  let 
    xVal = x1
    yVal = -y1 
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromFloat 1.0)
      , height (String.fromFloat 1.0)
      , fill "black" ]
      []

findCenter : List Pos -> Pos 
findCenter wholeRope =
  let 
    xSum = wholeRope |> List.map (Tuple.first) |> List.sum 
    ySum = wholeRope |> List.map (Tuple.second) |> List.sum
    len = wholeRope |> List.length
  in 
    (xSum // len, ySum // len)

toLocalSvg : Model -> Html Msg 
toLocalSvg model = 
  let 
    (head, tail) = model.rope
    (cx, cy) = findCenter (head :: tail)
    size = 20
    halfSize = size // 2
    (minX, maxX) = (cx - halfSize, cy + halfSize)
    (minY, maxY) = (cy - halfSize, cy + halfSize)
    viewBoxValues = [minX, 0 - maxY, size, size]
    viewBoxString = viewBoxValues |> List.map (String.fromInt) |> String.join " "
    vb = String.fromInt 
    lst = (tail |> List.map toTailCircle) ++ [ toHeadCircle head ]
  in 
    svg
      [ viewBox viewBoxString
      , width (String.fromInt 400)
      , height (String.fromInt 400) ]
--      , Svg.Attributes.style "background-color:lightblue" ]
      lst

toGlobalSvg : Model -> Html Msg 
toGlobalSvg model = 
  let 
    (head, tail) = model.rope
    lst = model.visited |> Set.toList |> List.map toVisitedRect
  in 
    svg
      [ viewBox "-260 -260 520 520"
      , width "400"
      , height "400" ]
--      , Svg.Attributes.style "background-color:lightblue" ]
      lst

view : Model -> Html Msg
view model =
  let
    s = if model.follow then toLocalSvg model else toGlobalSvg model
    movesStr = "moves: " ++ String.fromInt model.moves
    visitedStr = "visited: " ++ String.fromInt (Set.size model.visited)
    (head, tail) = model.rope 
    ropeLengthStr = "length: " ++ String.fromInt (1 + List.length tail)
  in 
    Html.table 
      [ Html.Attributes.style "width" "900px"
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2022" ]
              , Html.div [] [Html.text "Day 9: Rope Bridge" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2022/day/9" ] 
                [ text "https://adventofcode.com/2022/day/9" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ text "Step" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ text "Faster" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick ToggleZoom ] 
                [ if model.follow then text "Area" else text "Rope" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Shorter ] 
                [ text "Shorter" ]
              , text ("  " ++ ropeLengthStr ++ "  ")
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Longer ] 
                [ text "Longer" ] ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [] [ Html.text movesStr ]
              , Html.div [] [ Html.text visitedStr ]
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              ] ] ]
        
