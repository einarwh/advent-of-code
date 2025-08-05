module Main exposing (..)

import Browser 
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array2D exposing (Array2D)
import Html exposing (text)
import Time

defaultTickInterval : Float
defaultTickInterval = 40

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample 

type alias Acre = Char

type alias Pos = (Int, Int)

type alias Area = Array2D Acre 

type alias Model = 
  { area : Area
  , dataSource : DataSource
  , seen : Dict (List (List Acre)) Int
  , answer : Maybe Int
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

initArea : DataSource -> Area
initArea dataSource = 
  let 
    data = 
      case dataSource of 
        Sample -> sample 
        Input -> input 
  in 
    data |> String.split "\n" |> List.map (String.toList) |> Array2D.fromList

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    area = initArea dataSource
  in 
    { area = area
    , dataSource = dataSource
    , answer = Nothing
    , steps = 0 
    , seen = Dict.empty
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

getAllPositions : Array2D a -> List Pos
getAllPositions arr = 
  let
    ys = List.range 0 (Array2D.rows arr - 1)
    xs = List.range 0 (Array2D.columns arr - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

serialize : Array2D Acre -> List (List Acre)
serialize arr = 
  let
    ys = List.range 0 (Array2D.rows arr - 1)
    xs = List.range 0 (Array2D.columns arr - 1)
  in 
    ys |> List.map (\y -> xs |> List.filterMap (\x -> Array2D.get y x arr))

deserialize : List (List Acre) -> Array2D Acre 
deserialize = Array2D.fromList

getNestedPositions : Array2D a -> List (List Pos)
getNestedPositions arr = 
  let
    ys = List.range 0 (Array2D.rows arr - 1)
    xs = List.range 0 (Array2D.columns arr - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource 

posToString : (Int, Int) -> String 
posToString (x, y) = 
  String.fromInt x ++ "," ++ String.fromInt y

getNeighbours : Array2D a -> (Int, Int) -> List a
getNeighbours arr (x, y) = 
  let 
    positions = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1,y), (x+1,y), (x-1, y+1), (x, y+1), (x+1, y+1)]
  in 
    positions |> List.filterMap (\(xx, yy) -> Array2D.get yy xx arr)

getResourceValue : Area -> Int
getResourceValue area = 
  let 
    acres = area |> getAllPositions |> List.filterMap (\(x, y) -> Array2D.get y x area)
    trees = countAcre acres '|' 
    lumberyards = countAcre acres '#' 
  in 
    trees * lumberyards

countAcre : List Acre -> Acre -> Int 
countAcre acres acre = 
  acres |> List.filter (\a -> a == acre) |> List.length 

evolveAcre : Area -> Pos -> Acre 
evolveAcre area (x, y) = 
  let 
    neighbours = getNeighbours area (x, y) 
  in 
    case Array2D.get y x area |> Maybe.withDefault '.' of 
      '|' -> if countAcre neighbours '#' >= 3 then '#' else '|' 
      '#' -> if countAcre neighbours '#' > 0 && countAcre neighbours '|' > 0 then '#' else '.'
      _ -> if countAcre neighbours '|' >= 3 then '|' else '.' 

step : Area -> Area 
step area = 
  let 
    nestedPositions = getNestedPositions area
    nextArea = nestedPositions |> List.map (\positions -> positions |> List.map (evolveAcre area)) |> Array2D.fromList
  in 
    nextArea

updateStep : Model -> Model
updateStep model = 
  let  
    steps = model.steps
    key = model.area |> serialize
    (answer, seen, found) = 
      case model.answer of 
        Just _ -> (model.answer, model.seen, False) 
        Nothing -> 
          case Dict.get key model.seen of 
            Nothing -> 
              (Nothing, Dict.insert key steps model.seen, False)
            Just oldSteps -> 
              let 
                loopSize = steps - oldSteps
                numberInLoop = (1000000000 - oldSteps) |> modBy loopSize
                targetKey = model.seen |> Dict.toList |> List.filterMap (\(k, v) -> if v == oldSteps + numberInLoop then Just k else Nothing) |> List.head |> Maybe.withDefault []
                targetArea = deserialize targetKey
                resourceValue = getResourceValue targetArea
              in 
                (Just resourceValue, model.seen, True)
    a = step model.area 
    pause = steps + 1 == 10 || found
  in 
    { model | steps = steps + 1, area = a, answer = answer, seen = seen, paused = pause }

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

getAcreSymbolAtPos : Area -> Pos -> Char 
getAcreSymbolAtPos area (x, y) = 
  case Array2D.get y x area of 
    Just acre -> acre 
    Nothing -> '.'

toCharElement : Area -> Pos -> Html Msg 
toCharElement area (x, y) = 
  let 
    symbol = getAcreSymbolAtPos area (x, y) |> String.fromChar
  in  
    Html.text symbol

view : Model -> Html Msg
view model =
  let
    area = model.area
    nestedPositions = getNestedPositions area
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement area))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    resourceValue = getResourceValue area 
    prognosisStr =
      case model.answer of 
        Just prognosis -> String.fromInt prognosis
        Nothing -> "?"
    -- debugStr = model.seen |> Dict.size |> String.fromInt 
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
              [ Html.div [] [Html.text "Advent of Code 2018" ]
              , Html.div [] [Html.text "Day 18: Settlers of The North Pole" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2018/day/18" ] 
                [ Html.text "https://adventofcode.com/2018/day/18" ]
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
                [ Html.text "Reset"]
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
                Html.div [] [ Html.text ("Minutes: " ++ String.fromInt model.steps) ]
              , Html.div [] [ Html.text ("Value: " ++ String.fromInt resourceValue) ]
              , Html.div [] [ Html.text ("Prognosis: " ++ prognosisStr) ]
              -- , Html.div [] [ Html.text ("Debug: " ++ debugStr) ]
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
