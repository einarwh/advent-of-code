module Aoc18 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
import Html exposing (text)
import Time

defaultTickInterval : Float
defaultTickInterval = 100

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample 

type Acre = Ground | Trees | Lumberyard 

type alias Pos = (Int, Int)

type alias Area = Array2D Acre 

type alias Model = 
  { area : Area
  , dataSource : DataSource
  , steps : Int 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String
  , counter : Int 
  , debug : String }

sample : String
sample = """.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|."""

input : String
input = """.#||#|||##....|..#......|..#...##..|#....#|.......
|..#..#....|.#|.|......||.|..#|...||#......|.....|
..#|##.#.#.##...#..........#.#|...||.|..|##.#.|.||
|.#.#|#.#.||...|...|||#|.#..#|..|#.#..##.|......#|
#..|#|........|......##.|##..|..#|...#||.......|#.
#...|#..#......##...##.|......|.#|#.|..|#.|#...|.#
|#.....|.|.###..#...|....|..|.....|#..#..|.......#
.....##.|........|...#...|#..|..##...|......||.|..
#....#..|..#.........||.##..##|#.##.#....|...#.|.|
...|..#.|.|#||..|#.||.....#|.#|.|#|.....#|#.###|##
...|..#.||....||.#.|....|#...#|.||#.#..#...#...##.
...||.|#......|...|#...#..|...||..|.#|.....##.|||.
...|.#|.|#.|...#.....|.|...#|.|.........|||.|.##.|
..|..|#..|........#.|#.||.#|..#.|....||...|.|.#...
.|.|...#.|.#..........|..|........#|.|....|..|....
|...|.#..|..||#.||#........|...|.|.|..|.#|..|...|.
..#.#..#|......#|.#....###...#.#..|..|.....|....#.
..|||..#...|#|.##..#|#.#.#..|......#.....||.##.##.
...|...#.|##..|..|.|.#.|||#|......|.|..|.||#.#..||
||.....|..#|.#...|.|.#.||.....##...|.#...|#.#.##..
.|.|.#|..#........#..||.|.#|...###|.#..#........||
|.##......|.|||..|...##.|.....#|||....#...#||||.|.
...#...|||.......#..|.#.||.|.......|#|..|..#.|....
|..|#.............|...##|....|.#|..|#...|#...|.|..
|.|....|#...|##...#.....|..|..|...||#..|...|.#..||
...|.##.##....#.|#......##|...|..#.#....||||.||||.
||.#....#..#...|.||||##.....#..##......#..||##.#..
........#....|..#..#|#|....#..|..#.....##|...#.|..
..#.|#.|.#.#..#.....|..#...###....|#...........#.|
#.|#|.#...|.#.#.|..|....|..|.|.#|.#|#.............
.||......|||||...||.#......|#...|#.|.|..#.|.#|....
|.#|.#.|#.#..#.##......#.#|#.....#..#....#.##|.#..
#.#..|....###..|..|.||..|#..|...|...#|##....|#.#..
.|#...|..#|..#.|||.|..||...|..#.#...|..|#......#..
.##...#||..|#.#...|.......|.##.....|..|.#..|.#.|.#
#||##....#.|.||.#....|.#|..|.|.#....#..#...||.....
......||.#|........#....||.##...#....|.||...|..##|
#........|..#|.......#.#.#|..|...#..||||...|.....#
....#||.##....|..##...##|.....|..#.#.....#..|.....
.|.|#....|..|.#|#....#..|...|..#|#...#.||...#.#...
#....|.|#||....#.#|#|.#..|.#.....##........|...|#.
#...#...|..|.#....|..|.#....#.|#...#...#|.|.#.....
....#.......#....##|.#.|....##..|||##.....#|.....#
.....||||..|.#|#..|...|.#..|...#|...|.##||.#||....
.||....#...|..#.|#.#.|#|#|..#.........##...||..|#.
...|.#.#..........##...#|...|.##.|.|.||.#......#..
...###.#..|..#.....#|#.#.|#.######|.|#.....###.|.#
..##.....##...|..|....#|..||....|.|....#..|...|..#
.|.##.#...|.|.||.||.|#.#.....||#.#|.#|.|..#|.#..|#
.|...............#.#..#.##......#|||.|..||..#....#"""

parseAcre : Char -> Acre 
parseAcre ch = 
  case ch of 
    '#' -> Lumberyard
    '|' -> Trees 
    _ -> Ground 

initArea : DataSource -> Area
initArea dataSource = 
  let 
    data = 
      case dataSource of 
        Sample -> sample 
        Input -> input 
  in 
    data |> String.split "\n" |> List.map (String.toList >> List.map parseAcre) |> Array2D.fromList

toIndexedList : Array2D Bool -> List (Pos, Bool)
toIndexedList grid = 
  let 
    yRange = List.range 0 (Array2D.rows grid - 1)
    xRange = List.range 0 (Array2D.columns grid - 1)
  in 
    yRange |> List.concatMap (\y -> xRange |> List.map (\x -> ((x, y), Array2D.get y x grid |> Maybe.withDefault False)))

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    area = initArea dataSource
  in 
    { area = area
    , dataSource = dataSource
    , steps = 0 
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , counter = 0
    , message = ""
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Input, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | Faster 
  | Slower 
  | Clear 
  | UseSample 
  | UseInput 

getAllPositions : Array2D Bool -> List Pos
getAllPositions grid = 
  let
    ys = List.range 0 (Array2D.rows grid - 1)
    xs = List.range 0 (Array2D.columns grid - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

getNestedPositions : Array2D a -> List (List Pos)
getNestedPositions grid = 
  let
    ys = List.range 0 (Array2D.rows grid - 1)
    xs = List.range 0 (Array2D.columns grid - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource 

posToString : (Int, Int) -> String 
posToString (x, y) = 
  String.fromInt x ++ "," ++ String.fromInt y

isCorner : Array2D a -> (Int, Int) -> Bool
isCorner grid (x, y) = 
  let 
    xMax = Array2D.rows grid - 1
    yMax = Array2D.columns grid - 1
  in 
    (x == 0 && y == 0) || (x == xMax && y == 0) || (x == 0 && y == yMax) || (x == xMax && y == yMax)

getNeighbours : Array2D Bool -> (Int, Int) -> List Bool
getNeighbours grid (x, y) = 
  let 
    positions = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1,y), (x+1,y), (x-1, y+1), (x, y+1), (x+1, y+1)]
  in 
    positions |> List.filterMap (\(xx, yy) -> Array2D.get yy xx grid)

updateStep : Model -> Model
updateStep model = model 

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.dataSource
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateUseSample : Model -> Model
updateUseSample model = 
  initModel Sample 

updateUseInput : Model -> Model
updateUseInput model = 
  initModel Input 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    UseSample -> 
      (updateUseSample model, Cmd.none)
    UseInput -> 
      (updateUseInput model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toAcreSymbol : Acre -> Char 
toAcreSymbol acre = 
  case acre of 
    Lumberyard -> '#'
    Trees -> '|'
    Ground -> '.'

getAcreSymbolAtPos : Area -> Pos -> Char 
getAcreSymbolAtPos area (x, y) = 
  case Array2D.get y x area of 
    Just acre -> toAcreSymbol acre 
    Nothing -> '.'

toCharElement : Area -> Pos -> Html Msg 
toCharElement area (x, y) = 
  let 
    symbol = getAcreSymbolAtPos area (x, y) |> String.fromChar
  in  
    Html.text symbol

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2015 | Day 18: Like a GIF For Your Yard"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    area = model.area
    nestedPositions = getNestedPositions area
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement area))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    textFontSize = 
      case model.dataSource of 
        Sample -> "24px"
        Input -> "12px"
  in 
    Html.table 
      [ Html.Attributes.align "center"
      , Html.Attributes.style "width" "100%"
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2015" ]
              , Html.div [] [Html.text "Day 18: Like a GIF For Your Yard" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.text " ["
              , Html.a [ Html.Attributes.href "../../2024/"] [ Html.text "2024" ]
              , Html.text "] " 
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2023/"] [ Html.text "2023" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2022/"] [ Html.text "2022" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2021/"] [ Html.text "2021" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2020/"] [ Html.text "2020" ]
              , Html.text "] "
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2015/day/18" ] 
                [ Html.text "https://adventofcode.com/2015/day/18" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseInput, Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSample, Html.Attributes.checked (model.dataSource == Sample) ] 
                []
              , Html.label [] [ Html.text "Sample" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear"]
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
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px" ] 
              [ 
                Html.div [] [ Html.text ("Steps: " ++ String.fromInt model.steps) ]
              , Html.div [] [ Html.text "?" ]
              , Html.div [] [ Html.text model.message ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] 
              ]
