-- Advent of Code 2022. 
-- Day 14: Regolith Reservoir.

module Aoc14 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultDelay : Float
defaultDelay = 1000

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Pos = (Int, Int)

type alias Opacity = Float

type alias Layer = Int

type alias EndlessPos = (Pos, Opacity)

type Path = Endless (List EndlessPos) | Fixed (List Pos)

type alias Model = 
  { rocks : Set Pos
  , bottomRock: Int 
  , sand : Set Pos 
  , sandPath : Path 
  , hasFloor : Bool
  , done : Bool
  , delay : Float
  , paused : Bool
  , debug : String }

toPos : String -> Maybe Pos
toPos s = 
  case String.split "," s of 
    xStr :: yStr :: _ -> 
      case (String.toInt xStr, String.toInt yStr) of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing
    _ -> Nothing

pairwise : List a -> List (a, a) 
pairwise lst = 
  let
    skipped = List.drop 1 lst  
  in
    List.map2 Tuple.pair lst skipped  

toPositions : (Pos, Pos) -> List Pos 
toPositions (pos1, pos2) = 
  let
    (startPos, endPos) = if pos1 < pos2 then (pos1, pos2) else (pos2, pos1)
  in
    case (startPos, endPos) of 
      ((x1, y1), (x2, y2)) -> 
        if x1 == x2 then 
          List.range y1 y2 |> List.map (\y -> (x1, y))
        else 
          List.range x1 x2 |> List.map (\x -> (x, y1))

parseLine : String -> Set Pos 
parseLine s = 
  String.split " -> " s 
  |> List.filterMap toPos 
  |> pairwise 
  |> List.concatMap toPositions
  |> Set.fromList 

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""
    input = """441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
454,118 -> 454,122 -> 451,122 -> 451,126 -> 464,126 -> 464,122 -> 458,122 -> 458,118
440,135 -> 440,136 -> 453,136 -> 453,135
498,21 -> 502,21
469,69 -> 473,69
440,135 -> 440,136 -> 453,136 -> 453,135
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
455,72 -> 455,76 -> 452,76 -> 452,81 -> 464,81 -> 464,76 -> 459,76 -> 459,72
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
489,19 -> 493,19
469,65 -> 473,65
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
449,84 -> 453,84
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
438,158 -> 442,158
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
443,88 -> 447,88
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
470,50 -> 470,52 -> 469,52 -> 469,60 -> 478,60 -> 478,52 -> 474,52 -> 474,50
445,131 -> 454,131 -> 454,130
454,118 -> 454,122 -> 451,122 -> 451,126 -> 464,126 -> 464,122 -> 458,122 -> 458,118
454,118 -> 454,122 -> 451,122 -> 451,126 -> 464,126 -> 464,122 -> 458,122 -> 458,118
446,86 -> 450,86
463,65 -> 467,65
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
435,155 -> 439,155
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
457,69 -> 461,69
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
455,72 -> 455,76 -> 452,76 -> 452,81 -> 464,81 -> 464,76 -> 459,76 -> 459,72
495,19 -> 499,19
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
440,135 -> 440,136 -> 453,136 -> 453,135
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
455,72 -> 455,76 -> 452,76 -> 452,81 -> 464,81 -> 464,76 -> 459,76 -> 459,72
454,118 -> 454,122 -> 451,122 -> 451,126 -> 464,126 -> 464,122 -> 458,122 -> 458,118
470,50 -> 470,52 -> 469,52 -> 469,60 -> 478,60 -> 478,52 -> 474,52 -> 474,50
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
455,72 -> 455,76 -> 452,76 -> 452,81 -> 464,81 -> 464,76 -> 459,76 -> 459,72
472,67 -> 476,67
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
470,50 -> 470,52 -> 469,52 -> 469,60 -> 478,60 -> 478,52 -> 474,52 -> 474,50
439,91 -> 439,94 -> 435,94 -> 435,98 -> 444,98 -> 444,94 -> 443,94 -> 443,91
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
495,14 -> 506,14 -> 506,13
429,155 -> 433,155
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
470,50 -> 470,52 -> 469,52 -> 469,60 -> 478,60 -> 478,52 -> 474,52 -> 474,50
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
455,72 -> 455,76 -> 452,76 -> 452,81 -> 464,81 -> 464,76 -> 459,76 -> 459,72
439,91 -> 439,94 -> 435,94 -> 435,98 -> 444,98 -> 444,94 -> 443,94 -> 443,91
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
445,131 -> 454,131 -> 454,130
452,86 -> 456,86
455,88 -> 459,88
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
441,114 -> 441,115 -> 455,115
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
460,67 -> 464,67
470,50 -> 470,52 -> 469,52 -> 469,60 -> 478,60 -> 478,52 -> 474,52 -> 474,50
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
492,21 -> 496,21
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
439,91 -> 439,94 -> 435,94 -> 435,98 -> 444,98 -> 444,94 -> 443,94 -> 443,91
470,50 -> 470,52 -> 469,52 -> 469,60 -> 478,60 -> 478,52 -> 474,52 -> 474,50
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
492,17 -> 496,17
455,72 -> 455,76 -> 452,76 -> 452,81 -> 464,81 -> 464,76 -> 459,76 -> 459,72
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
454,118 -> 454,122 -> 451,122 -> 451,126 -> 464,126 -> 464,122 -> 458,122 -> 458,118
466,67 -> 470,67
470,50 -> 470,52 -> 469,52 -> 469,60 -> 478,60 -> 478,52 -> 474,52 -> 474,50
466,63 -> 470,63
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
486,21 -> 490,21
455,72 -> 455,76 -> 452,76 -> 452,81 -> 464,81 -> 464,76 -> 459,76 -> 459,72
439,91 -> 439,94 -> 435,94 -> 435,98 -> 444,98 -> 444,94 -> 443,94 -> 443,91
481,34 -> 481,32 -> 481,34 -> 483,34 -> 483,27 -> 483,34 -> 485,34 -> 485,32 -> 485,34 -> 487,34 -> 487,26 -> 487,34
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
426,158 -> 430,158
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
441,114 -> 441,115 -> 455,115
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
475,69 -> 479,69
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
454,118 -> 454,122 -> 451,122 -> 451,126 -> 464,126 -> 464,122 -> 458,122 -> 458,118
463,69 -> 467,69
432,158 -> 436,158
439,91 -> 439,94 -> 435,94 -> 435,98 -> 444,98 -> 444,94 -> 443,94 -> 443,91
454,118 -> 454,122 -> 451,122 -> 451,126 -> 464,126 -> 464,122 -> 458,122 -> 458,118
439,91 -> 439,94 -> 435,94 -> 435,98 -> 444,98 -> 444,94 -> 443,94 -> 443,91
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
432,152 -> 436,152
439,91 -> 439,94 -> 435,94 -> 435,98 -> 444,98 -> 444,94 -> 443,94 -> 443,91
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
473,47 -> 473,42 -> 473,47 -> 475,47 -> 475,45 -> 475,47 -> 477,47 -> 477,39 -> 477,47 -> 479,47 -> 479,45 -> 479,47 -> 481,47 -> 481,42 -> 481,47 -> 483,47 -> 483,46 -> 483,47 -> 485,47 -> 485,39 -> 485,47
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111
495,14 -> 506,14 -> 506,13
435,149 -> 435,148 -> 435,149 -> 437,149 -> 437,146 -> 437,149 -> 439,149 -> 439,146 -> 439,149 -> 441,149 -> 441,145 -> 441,149
449,88 -> 453,88
441,111 -> 441,105 -> 441,111 -> 443,111 -> 443,104 -> 443,111 -> 445,111 -> 445,106 -> 445,111 -> 447,111 -> 447,104 -> 447,111"""

    lines = input |> String.split "\n"
    rocks = lines
            |> List.map parseLine
            |> List.map (Set.toList)
            |> List.concat
            |> Set.fromList

    bottomRock = rocks |> Set.toList |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0

    model = { rocks = rocks
            , bottomRock = bottomRock
            , sandPath = Fixed []
            , sand = Set.empty
            , hasFloor = False
            , done = False
            , delay = defaultDelay
            , paused = True
            , debug = ".." }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick | Step | TogglePlay | Faster | Slower 

tryFind : (a -> Bool) -> List a -> Maybe a
tryFind pred lst = 
  case lst of 
    [] -> Nothing 
    h :: t -> 
      if pred h then Just h else tryFind pred t

createTrailingPath : Pos -> List EndlessPos 
createTrailingPath (x, y) = 
  List.range 0 9 -- [0..9]
  |> List.map (\i -> ((x, y + i), 1.0 - toFloat i * 0.1))

buildSandPath : Bool -> Int -> Pos -> List Pos -> Set Pos -> Path
buildSandPath hasFloor bottomRock (x, y) path occupied = 
  if y == bottomRock && not hasFloor then 
    let 
      basePath = path |> List.map (\p -> (p, 1.0))
      endless = (List.reverse basePath) ++ createTrailingPath (x, y)
    in 
      Endless endless
  else 
    let
      candidates = 
        if y == bottomRock + 1 && hasFloor then 
          []
        else 
          [ (x, y + 1), (x - 1, y + 1), (x + 1, y + 1) ]
      maybe = candidates |> tryFind (\c -> not <| Set.member c occupied)
    in 
      case maybe of
        Just nextPos -> 
          buildSandPath hasFloor bottomRock nextPos ((x, y) :: path) occupied 
        Nothing -> 
          let
            finishedPath = (x, y) :: path
          in 
            finishedPath |> List.reverse |> Fixed

findSandPath : Bool -> Int -> Set Pos -> Path 
findSandPath hasFloor bottomRock occupied =  
  let 
    startPos = (500, 0)
  in 
    buildSandPath hasFloor bottomRock startPos [] occupied

updateModel : Model -> Model
updateModel model = 
  case model.sandPath of 
    Endless [] -> 
      { model | debug = "Endless []" }
    Endless (h :: t) -> 
      { model | sandPath = Endless (t ++ [h]), debug = "Endless (h :: t)" }
    Fixed [] -> 
      let 
        occupied = Set.union model.rocks model.sand
        sandPath = findSandPath model.hasFloor model.bottomRock occupied 
      in 
        { model | sandPath = sandPath, debug = "Fixed []" }
    Fixed [ last ] -> 
      let
        sand = Set.insert last model.sand
        sandPath = Fixed []
      in
        { model | sandPath = sandPath, sand = sand, debug = "Fixed [last]" }
    Fixed (_ :: t ) -> 
      let
        sandPath = Fixed t 
      in
        { model | sandPath = sandPath, debug = "Fixed (h :: t)" }

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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then Sub.none 
  else Time.every model.delay (\_ -> Tick)

-- VIEW

toOccupiedElement : String -> Float -> Pos -> Html Msg 
toOccupiedElement color elemOpacity (posX, posY) = 
  let 
    w = 1
    h = 1
    xOffset = 0
    yOffset = 0
    xVal = xOffset + posX * w
    yVal = yOffset + posY * h
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt w) 
      , height (String.fromInt h)
      , stroke "none"
      , fill color
      , opacity (String.fromFloat elemOpacity) ]
      []

toRockElement : Pos -> Html Msg 
toRockElement pos = toOccupiedElement "black" 1.0 pos

toSandElement : Pos -> Html Msg 
toSandElement pos = toOccupiedElement "#C2B280" 1.0 pos

toFixedElement : Pos -> Html Msg 
toFixedElement pos = toOccupiedElement "#C19A6B" 1.0 pos

toEndlessElement : EndlessPos -> Html Msg 
toEndlessElement (pos, opacity) = toOccupiedElement "#C19A6B" opacity pos

toGrainElements : Path -> List (Html Msg)
toGrainElements path = 
  case path of 
    Endless [] -> []
    Endless (h :: _) -> [ toEndlessElement h ]
    Fixed [] -> []
    Fixed (h :: _) -> [ toFixedElement h ]

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    rockElements = model.rocks |> Set.toList |> List.map toRockElement
    sandElements = model.sand |> Set.toList |> List.map toSandElement
    grainElements = model.sandPath |> toGrainElements
  in 
    svg
      [ viewBox "300 0 400 160"
      , width "800"
      , height "320" 
      ]
      (rockElements ++ sandElements ++ grainElements)

viewHtml : Model -> Html Msg
viewHtml model =
  let
    s = toSvg model
    debugStr = model.debug
    sandStr = model.sand |> Set.size |> String.fromInt
  in 
    Html.table 
      [ 
        Html.Attributes.style "font-family" "Courier New"
      ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2022" ]
              , Html.div [] [Html.text "Day 14: Regolith Reservoir" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text ("Sand: " ++ sandStr) ]
              -- , Html.div [] [ Html.text debugStr ]
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
                [ text "Faster" ] ] ] ] 

view : Model -> Browser.Document Msg
view model = 
  { title = "Advent of Code 2022 - Day 14: Regolith Reservoir"
  , body = [ viewHtml model ] }

