module Main exposing (..)

{- Advent of Code 2020. Day 3: Toboggan Trajectory. -}

import Browser 
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
defaultTickInterval = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type alias Pos = (Int, Int)

type Dir = N | W | S | E

type Move = Turn | Forward

type Visit = Vertical | Horizontal | Both 

type Cell = Highlight Char | Plain Char 

type alias Toboggan =
  { slope : Pos
  , pos : Pos
  , crashes : Int }

type alias Model = 
  { area : Array2D Char
  , allSlopes : Bool 
  , toboggans : List Toboggan 
  , xMax : Int 
  , yMax : Int 
  , dataSource : DataSource
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String }

sample : String
sample = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""

input : String 
input = """....#...............##...#...#.
#...#..#.....##.##.#.##....#...
...#.....#...#.................
#..#..#.......#...#.#..........
...##..#.#..........##...#.....
........###.#.##..#............
...###......##.#..#.#...#.#....
......##..#.#....#...#.........
.................#......#......
..............##....#..........
#.....................#...#.#.#
.##..#............##...##.##..#
.....#.####...#..##......#.#..#
#.......#.#..#......##.#.#....#
.....##...###.#..........##....
#...........#.##....##.....#..#
..###..##.##.....#....#........
...#.#.#............#.#..#....#
#......#....#...##.#.#.#.#..#..
.......#.#...#..#..#..##......#
.....#..#.............#..#...#.
##..#.##.....#........#........
....##....#..#...........#...#.
.......#........##.......##....
..##...#.......#........##.#...
..........#..#.....##........#.
..#..##..#............#........
.#.#...#...#.......#......#....
....#....#.....#.#.........###.
.............#...#....#..#.#...
##.#...#..#......#.#.##.....#..
#...##.#..........#..#.#...#...
#####.......#.#.....#..#.......
#...#...#........#....#...#....
......##.#..#..#............#..
....#....#.......#...###.......
.#......##...#.##....#...#.....
..#....#...##.....#.#...##.#...
#.......#........#.####........
#.##..#..#.........#.#........#
.#...#.#.#.#......#....#.#..#..
#...####...##.##.#....#......##
..#...#......##........#.....#.
...#.#....##...................
...##................#.........
...##.....##........#....#..#..
.........#..#.....#............
.#..#.......................#..
.#.........#..##........#.#.#.#
......#.....##..#.##...#..#.##.
..#..............##.......#....
...............##....#...##..#.
###...#..###.........#...#.....
...#..#...#....#.....##........
....#..##...#........#.........
..#......#.......#.....#..#....
.#...........##.....###....#...
.#..#.....##.........##.....#..
....##.#.....#................#
..#..#......#.#..#....#..##....
#.....#...##............#......
.#.............#....#.......#..
#.........#..#...##.#...#.#.##.
...#......#..####....#.#.....#.
......#........#..........##.##
......##.#..##.##.....#........
##.....#..##.##..#.......##....
.##.........#...........#...#..
.....#...###..#...#...........#
..........#.#......#.###.....#.
...#.............#.##......##..
#.##.........#..###...........#
....#..##....#..#..#........###
...#........##.......##..#..#..
...#......#..#.#...............
#......###....#.#..#.#..#....#.
#.#.####.#.........#..#.#.#....
.....#....#...............#...#
.#........#......#.#...#.......
................#...#.....##...
.............#...####..........
.................##....##.###..
#................#......#......
.###.#........#..##.....#..###.
..#.#..#...#..#.#...#.#.....#.#
.....#............#..##..#..#.#
#........##.#...#.....#........
#.#.#..###......###............
...#..#...........##...#.....#.
......#........#...#.#....#....
....#..........#.#..#.#....##..
...#.....##..#......#.#.##...#.
.........#..#................#.
..#....#.##.....#.......#......
...#.....#.......##.##.....#...
#...#..............#..###..#...
#.#......#.#....#........##..#.
...#...##...##..#.........#....
..#...#......##.#.#.#....##....
#.......#.......#..#..#........
.........#..#.....#....#.....##
.#......#.......#.#..#..#...#.#
..#....#.#..#..................
#.....####..........#.#.....#.#
.#..#.#.#....#.#.....#.#.......
....##......#..#.....#.#.#...#.
...##...#......##.#....##.#....
..#..##....#...#...........#...
.......#........#...##.#.......
#.#..#....##.#....##...........
.......#............#..##..##..
#.#.#.....#....##.#.#.#.....#..
##...#...#.......#..#...#.....#
##..##.##..........#........##.
..............#.....#..#..##...
.......#...#.........#....#.#..
...#..#..#....#.#....##........
..#.......#....#....##.........
#...#.....#..#.#...##....#.....
.....##..#..##..#..............
.....##............#....#.#....
..#.....#....##.#.....#..#.....
#...#..#..#......#.#.#..##.....
.............................##
#...#.#................#....#.#
.#.#.#....##......###..##......
#.....#..#.##.#.#.##...###.....
.........#............##..#....
.#..#...#....#.....#.#........#
...............#......#..#.....
...................###........#
.###..##..##.......#.#.........
#.........#......#....#.#...#..
.#.#....#.......#.#..##...##...
.#.....#....##.......#.#.....#.
.........#...#....#.#..........
....###..#..##.#...##....#..#..
...#.#..##.#.........###.#..#..
#...#...........#....#.........
....##...........#.#.#......###
#....#...........##..#.........
###....#.....#.......#....###..
.#.......#....#.#.#.#......#.#.
........#...............#.#.#..
....#.........#.....#...##.##.#
...#............#.............#
..........#..#.................
........#.....##............#.#
..#...##........#...#.....#.#..
....#........#.#.#..........#..
#.#...#...........#............
....#.#...##...........#.....#.
...........#.#..#.....#........
.....#..#..#..#.....#.#.....#.#
#.....#.......#.......#...#....
#.........#....#.#........#..#.
...#..#.........#.....#..#.....
...#..#.............#..........
.#.......#..........#.....#...#
.....#.#......#.......#....#...
...#.....#..#..##....##....#...
.#.#.#..#...#.....#....#.......
..##.#..........#.....#.#......
..#..#.............#...##..##..
.#.............#..#....##...#..
..#...#.....#.................#
..##.......#.....#...#....#....
.#..#.##.........#...#.#...#...
...##.......##..#.....##.##...#
........####.#.........#.......
..#.#...##.#..#..#.......##.#..
.#..#............###..#..#.....
#.....#.#...#.#.......#........
..........#......#.#...#...#...
..#......#..#..#.#...#.........
..###........#.#....#.#...##...
.#.....#..#.#......#........#..
.#...#..#...#....#.......#..#..
..#....#..#.....#.#........#...
#..#.#.........#..........#..#.
.#.....##....#.........#.#.#.#.
#.#...#.....#.#.#....#.#..#....
.........#...................#.
..#.....#..##...#..........#.#.
..............#....#.........#.
.#....#.....#..............##..
#...#...#.#........##.........#
....###....#.#....#.#.........#
.....#........#.....##.........
.#...##..##..#.........##......
............#.....#........#...
..#....#.......#......#..#.#.#.
#.......#.#...........#..##.#..
......#.##......#....#.......#.
.....#........#...###.....#....
###..........#........#.#.#....
.....#...#.#...#...#...##.....#
.##...#.#........#.#....#......
......#.........#.....#.#......
.....#.##.....###.#...#...##..#
.#.#.......##....#..#..#.##....
.####...###.#.#.#.#............
......#..##...#..........#.##.#
......#............#...........
.....#.#..#.......##...##......
......#........#..#....#.#.#.#.
#..#..#.....#..#.....#.......#.
.#...#.....#..............#....
.#....#..#.##.#............####
..........#....#.##...#.#......
...#.#.#.#.#.......#.........#.
##........#..##..#.........#...
..#......#...#..#.#.....#......
..#.#......#...#...#.#.........
........................##.....
...#.##.#........#...#.......#.
..#.#......#....##........#.#..
#......#.##........#..#......#.
.....#..#..#.............#.....
......#......#........#....#...
...#....###.....#..#.#....#....
#.......................#....#.
..#...#...................#....
....#..#.....##.#..#...#.....#.
...#.........#...#.......#.....
..#....#.....#...#...#.#####...
.....####......#...........#...
......#.#..........#...#.#.#..#
###..#.#....#..#...............
...#...###..#..#.#.#...........
.....#...#.##.#.#.###..##......
.........#...........#....##.#.
....#..#......#................
...........#..#..#...#.#.......
..#.....#......##.###..........
.........#...................#.
..........#...#.#....##........
..#...##....#....#.......#...##
#......#.....#...#...#...#.....
....##...#.#.......#.#...##....
...#.....#....#.....#....#.....
#....##.....##..##..........##.
.....#.....#.#.#...............
.#.##....#.....#.#..#....#..##.
.....#.#.....##....#...........
.........#..#.......##..##.....
..#....##.....###...#....#.#...
............#......#.#...#..#..
#..##......#.#.##....#.#.......
.#.#.....#...#.#.#....#.....#..
#....#..#.#....#...#...........
......#.#.....#...#.#.#......#.
###..#....#.###.............#..
..............#####........###.
..#..#.#.#.#......#......#.....
###.........#.#..........#..#.#
.#.........#...#......####.....
..#.......####..#....#...#..#..
#.#..#.#...............#.#.#.#.
###....#.....##.#....#......##.
..#..#........#....###.#.#.....
...#.#..........#.....#...#....
....#......##.#............#..#
...##...#.....#..##....#..#.#.#
.......#.....#..#....#....##.#.
.#..#....#..#......##....##...#
..#......#...#.#..###..#.##....
#...#.....#......##...#.......#
.....#.#.....#...##............
.#..##.##..#..##.#........#....
....#.#......##...#.#.#.#..##..
.#..............##........#....
.##....#..#..#....#...#......#.
............###....##.......##.
..............####.....#.......
........##..##.#...#.......#...
....#..#.....##.......#####...#
.##..##..#.....#...#..#..#....#
##..#.#.#...........#..........
#..#......#...#....#...........
...#..##.#..........#..#.......
........#.#.....#......##......
.....#....#............#.......
.#.#..#....##......#.......###.
.#..#.#........#......#...##..#
...#....#......#..#........#.##
.........#..#...#..#.#.##......
....###.#...........#...#......
.##............#.......#..##...
##...#.#...............#.#...##
..#..#.....#.#..#..#...........
..#..#.##..#......#.##..#.#....
..#...#......#.#...#....##.#...
...###....####......#....#...#.
.......##........#.....##....#.
.........##..........#...#.....
.....#............#.##.#....#.#
..........#...#....##..........
....................#......#...
#......#..#...#.............##.
...........#...................
..#...#.........#.##.#..##.#...
#.#....#.#.....#............#..
.#..#.....#.....####......#.#..
#....#.......##..#...........#.
............#...#.....#..#.#...
#...........#...#####....#...#.
..........#...###..##.........#
#.....###............#..#..#.#.
...##.....#....#......#.....#..
#....#.......#..#......###...#.
...##.##......##..##..........#
.......#.#..#.#..#.#.#.#..#..#.
..#..###...#....#.....#......#.
...#.........#..#.##.#.....###.
..#.........#.##.#..#..#..###..
..####..#.........#.........#.#
..#.#...#.......#....##........
.#......#.#....................
..........#.......#.#..#..#....
..#........#....#.#..#.........
..#.....#.............#....#...
##...#.........#.....#...#....."""

initArea : DataSource -> Array2D Char
initArea dataSource = 
  let 
    data = 
      case dataSource of 
        Sample -> sample 
        Input -> input 
  in 
    data |> String.split "\n" |> List.map (String.toList) |> Array2D.fromList

initModel : Bool -> DataSource -> Model 
initModel allSlopes dataSource = 
  let 
    area = initArea dataSource
    toboggans = [ { slope = (3, 1), pos = (0, 0), crashes = 0 }]
    xMax = Array2D.columns area
    yMax = Array2D.rows area 
  in 
    { area = area
    , allSlopes = allSlopes
    , toboggans = toboggans
    , xMax = xMax
    , yMax = yMax 
    , dataSource = dataSource
    , paused = True
    , finished = False  
    , tickInterval = defaultTickInterval 
    , message = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False Input, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleWide 
  | Faster 
  | Slower 
  | Clear 
  | UseDataSource DataSource

getAllPositions : Array2D Char -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

getNestedPositions : Array2D Char -> List (List Pos)
getNestedPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.allSlopes model.dataSource

isCrash : Array2D Char -> Pos -> Bool 
isCrash area (x, y) = 
  case Array2D.get y x area of 
    Just '#' -> True 
    Just 'X' -> True 
    _ -> False 

replaceToboggan : Toboggan -> List Toboggan -> List Toboggan
replaceToboggan toboggan toboggans = 
  toboggans |> List.map (\t -> if t.slope == toboggan.slope then toboggan else t)

updateToboggan : Toboggan -> Model -> Model 
updateToboggan toboggan model =
  let
    (x, y) = toboggan.pos 
    (dx, dy) = toboggan.slope 
    crashes = toboggan.crashes
  in
    if y < model.yMax then 
      let 
        xNext = x+dx |> modBy model.xMax
        yNext = y+dy 
        movedToboggan = { toboggan | pos = (xNext, yNext) }
        toboggans = model.toboggans
      in 
      if isCrash model.area (x, y) then
        let 
          area = Array2D.set y x 'X' model.area
          crashedToboggan = { movedToboggan | crashes = crashes + 1 }
        in 
          { model | toboggans = replaceToboggan crashedToboggan toboggans, area = area }
      else 
        { model | toboggans = replaceToboggan movedToboggan toboggans }
    else 
      model 

updateStep : Model -> Model
updateStep model = 
  model.toboggans |> List.foldl (\t m -> updateToboggan t m) model

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateToggleWide : Model -> Model
updateToggleWide model = 
  initModel (not model.allSlopes) model.dataSource 

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
    ToggleWide -> 
      (updateToggleWide model, Cmd.none)
    UseDataSource dataSource -> 
      (initModel model.allSlopes dataSource, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    Sub.batch [ tickSub ] 

-- VIEW

toWarehouseRows : Array2D Char -> List String 
toWarehouseRows warehouse = 
  let
    ys = List.range 0 (Array2D.rows warehouse - 1)
    xs = List.range 0 (Array2D.columns warehouse - 1)
    rows = ys |> List.map (\y -> xs |> List.filterMap (\x -> Array2D.get y x warehouse) |> String.fromList)
  in 
    rows 

toRowElements : String -> List (Html Msg)
toRowElements rowText = 
  [ Html.text rowText, Html.br [] [] ]

toCharElement : Array2D Char -> List Pos -> Pos -> Html Msg 
toCharElement area tobogganPositions (x, y) = 
  if List.member (x, y) tobogganPositions then 
    Html.span [Html.Attributes.class "hero adaptive" ] [ Html.text (String.fromChar '@') ] 
  else 
    case Array2D.get y x area of 
      Nothing -> Html.text "?"
      Just symbol -> 
        case symbol of 
          '#' -> 
            Html.span [Html.Attributes.class "draw-dark-green adaptive" ] [ Html.text (String.fromChar symbol) ] 
          'X' -> 
            Html.span [Html.Attributes.class "wrong adaptive" ] [ Html.text (String.fromChar symbol) ] 
          _ -> 
            Html.span [Html.Attributes.class "draw-empty adaptive" ] [ Html.text (String.fromChar symbol) ] 

view : Model -> Html Msg
view model =
  let
    crashScore = model.toboggans |> List.map (\t -> t.crashes) |> List.foldl (\n p -> n * p) 1
    textFontSize = 
      case model.dataSource of 
        Input -> "16px"
        Sample -> "32px"
    tobogganPositions = model.toboggans |> List.map (\t -> t.pos)
    nestedPositions = getNestedPositions model.area
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement model.area tobogganPositions))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []

  in 
    Html.table 
      [ Html.Attributes.style "width" "1080px"
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2020" ]
              , Html.div [] [Html.text "Day 3: Toboggan Trajectory" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2020/day/3" ] 
                [ text "https://adventofcode.com/2020/day/3" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick (UseDataSource Input), Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick (UseDataSource Sample), Html.Attributes.checked (model.dataSource == Sample) ] 
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
                [ text "Reset" ]
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
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleWide, Html.Attributes.checked model.allSlopes ] 
                []
              , Html.label [] [ Html.text " all slopes" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt crashScore) ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] elements
              ] ] ]
