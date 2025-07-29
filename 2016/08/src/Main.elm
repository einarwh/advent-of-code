module Main exposing (..)

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

defaultDelay : Float
defaultDelay = 60

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

type alias Screen = Array2D Bool

type Operation = Rect (Int, Int) | RotateRow (Int, Int) | RotateColumn (Int, Int)

type alias Model = 
  { lit : Int 
  , screen : Screen
  , prevOperation : Maybe Operation
  , operations : List Operation 
  , dataSource : DataSource 
  , delay : Float
  , paused : Bool  
  , lastCommandText : String
  , counter : Int 
  , debug : String }

tryParseDim : String -> Maybe (Int, Int)
tryParseDim s = 
  let 
    parts = s |> String.split "x"
  in 
    case parts of 
      wStr :: hStr :: _ -> 
        case (String.toInt wStr, String.toInt hStr) of 
          (Just w, Just h) -> Just (w, h)
          _ -> Nothing
      _ -> Nothing 

tryParseRect : String -> Maybe Operation
tryParseRect s = 
  -- rect 3x2
  if s |> String.startsWith "rect" then 
    let 
      parts = s |> String.split " "
    in 
      case parts of 
        _ :: dim :: _ -> tryParseDim dim |> Maybe.map Rect
        _ -> Nothing 
  else 
    Nothing

tryParseRotate : String -> Maybe (Int, Int)
tryParseRotate s = 
  -- rotate ... y=0 by 4
  case s |> String.split " " of 
    _ :: _ :: eqStr :: _ :: stepStr :: _ -> 
      case eqStr |> String.split "=" of 
        _ :: indexStr :: _ -> 
          case (String.toInt indexStr, String.toInt stepStr) of 
            (Just index, Just step) -> Just (index, step)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing 

tryParseRotateRow : String -> Maybe Operation
tryParseRotateRow s = 
  -- rotate row y=0 by 4
  if s |> String.startsWith "rotate row" then 
    s |> tryParseRotate |> Maybe.map RotateRow
  else 
    Nothing

tryParseRotateColumn : String -> Maybe Operation
tryParseRotateColumn s = 
  -- rotate column x=1 by 1
  if s |> String.startsWith "rotate column" then 
    s |> tryParseRotate |> Maybe.map RotateColumn
  else 
    Nothing

parseOp : String -> Maybe Operation 
parseOp s = 
  case s |> tryParseRect of 
    Just op -> Just op 
    Nothing -> 
      case s |> tryParseRotateRow of 
        Just op -> Just op 
        Nothing -> 
          s |> tryParseRotateColumn 

parseOperations : String -> List Operation
parseOperations s = 
  s |> String.split "\n" |> List.filterMap parseOp

initOperations : DataSource -> List Operation
initOperations dataSource = 
  let 
    sample = """rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1"""
    input = """rect 1x1
rotate row y=0 by 7
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 3
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 3
rect 1x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 7
rect 6x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 2
rect 1x2
rotate row y=1 by 10
rotate row y=0 by 3
rotate column x=0 by 1
rect 2x1
rotate column x=20 by 1
rotate column x=15 by 1
rotate column x=5 by 1
rotate row y=1 by 5
rotate row y=0 by 2
rect 1x2
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate row y=2 by 15
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate row y=2 by 5
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate row y=2 by 10
rotate row y=0 by 10
rotate column x=8 by 1
rotate column x=5 by 1
rotate column x=0 by 1
rect 9x1
rotate column x=27 by 1
rotate row y=0 by 5
rotate column x=0 by 1
rect 4x1
rotate column x=42 by 1
rotate column x=40 by 1
rotate column x=22 by 1
rotate column x=17 by 1
rotate column x=12 by 1
rotate column x=7 by 1
rotate column x=2 by 1
rotate row y=3 by 10
rotate row y=2 by 5
rotate row y=1 by 3
rotate row y=0 by 10
rect 1x4
rotate column x=37 by 2
rotate row y=3 by 18
rotate row y=2 by 30
rotate row y=1 by 7
rotate row y=0 by 2
rotate column x=13 by 3
rotate column x=12 by 1
rotate column x=10 by 1
rotate column x=7 by 1
rotate column x=6 by 3
rotate column x=5 by 1
rotate column x=3 by 3
rotate column x=2 by 1
rotate column x=0 by 1
rect 14x1
rotate column x=38 by 3
rotate row y=3 by 12
rotate row y=2 by 10
rotate row y=0 by 10
rotate column x=7 by 1
rotate column x=5 by 1
rotate column x=2 by 1
rotate column x=0 by 1
rect 9x1
rotate row y=4 by 20
rotate row y=3 by 25
rotate row y=2 by 10
rotate row y=0 by 15
rotate column x=12 by 1
rotate column x=10 by 1
rotate column x=8 by 3
rotate column x=7 by 1
rotate column x=5 by 1
rotate column x=3 by 3
rotate column x=2 by 1
rotate column x=0 by 1
rect 14x1
rotate column x=34 by 1
rotate row y=1 by 45
rotate column x=47 by 1
rotate column x=42 by 1
rotate column x=19 by 1
rotate column x=9 by 2
rotate row y=4 by 7
rotate row y=3 by 20
rotate row y=0 by 7
rotate column x=5 by 1
rotate column x=3 by 1
rotate column x=2 by 1
rotate column x=0 by 1
rect 6x1
rotate row y=4 by 8
rotate row y=3 by 5
rotate row y=1 by 5
rotate column x=5 by 1
rotate column x=4 by 1
rotate column x=3 by 2
rotate column x=2 by 1
rotate column x=1 by 3
rotate column x=0 by 1
rect 6x1
rotate column x=36 by 3
rotate column x=25 by 3
rotate column x=18 by 3
rotate column x=11 by 3
rotate column x=3 by 4
rotate row y=4 by 5
rotate row y=3 by 5
rotate row y=2 by 8
rotate row y=1 by 8
rotate row y=0 by 3
rotate column x=3 by 4
rotate column x=0 by 4
rect 4x4
rotate row y=4 by 10
rotate row y=3 by 20
rotate row y=1 by 10
rotate row y=0 by 10
rotate column x=8 by 1
rotate column x=7 by 1
rotate column x=6 by 1
rotate column x=5 by 1
rotate column x=3 by 1
rotate column x=2 by 1
rotate column x=1 by 1
rotate column x=0 by 1
rect 9x1
rotate row y=0 by 40
rotate column x=44 by 1
rotate column x=35 by 5
rotate column x=18 by 5
rotate column x=15 by 3
rotate column x=10 by 5
rotate row y=5 by 15
rotate row y=4 by 10
rotate row y=3 by 40
rotate row y=2 by 20
rotate row y=1 by 45
rotate row y=0 by 35
rotate column x=48 by 1
rotate column x=47 by 5
rotate column x=46 by 5
rotate column x=45 by 1
rotate column x=43 by 1
rotate column x=40 by 1
rotate column x=38 by 2
rotate column x=37 by 3
rotate column x=36 by 2
rotate column x=32 by 2
rotate column x=31 by 2
rotate column x=28 by 1
rotate column x=23 by 3
rotate column x=22 by 3
rotate column x=21 by 5
rotate column x=20 by 1
rotate column x=18 by 1
rotate column x=17 by 3
rotate column x=13 by 1
rotate column x=10 by 1
rotate column x=8 by 1
rotate column x=7 by 5
rotate column x=6 by 5
rotate column x=5 by 1
rotate column x=3 by 5
rotate column x=2 by 5
rotate column x=1 by 5"""
    data = 
      case dataSource of 
        Sample -> sample
        Input -> input
  in 
    parseOperations data 

initScreen : DataSource -> Screen
initScreen dataSource = 
  let 
    (cols, rows) = 
      case dataSource of 
        Sample -> (7, 3) 
        Input -> (50, 6)
  in 
    Array2D.initialize rows cols (\y -> \x -> False)  

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    operations = initOperations dataSource 
    screen = initScreen dataSource
    model = { lit = 0
            , operations = operations
            , prevOperation = Nothing
            , screen = screen
            , delay = defaultDelay
            , paused = True
            , lastCommandText = "press play to start"
            , dataSource = dataSource 
            , counter = 0
            , debug = "" }
  in 
    model

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Input, Cmd.none)

-- UPDATE 

type Msg = Tick | Step | TogglePlay | Faster | Slower | Clear | UseSample | UseInput

updateClear : Model -> Model
updateClear model = initModel model.dataSource

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  { model | dataSource = dataSource, lit = 0, operations = initOperations dataSource, screen = initScreen dataSource } 

updateUseSample : Model -> Model
updateUseSample model = 
  updateDataSource Sample model 

updateUseInput : Model -> Model
updateUseInput model = 
  updateDataSource Input model 

getPositions : Int -> Int -> List Pos
getPositions cols rows = 
  let
    ys = List.range 0 (rows - 1)
    xs = List.range 0 (cols - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

turnOn : List Pos -> Screen -> Screen 
turnOn posList screen =
  case posList of 
    [] -> screen 
    (x, y) :: rest -> 
      screen |> Array2D.set y x True |> turnOn rest

rect : (Int, Int) -> Screen -> Screen 
rect (w, h) screen = 
  let 
    posList = getPositions w h 
  in 
    turnOn posList screen

rotateRowLoop : Int -> Int -> Int -> Int -> Screen -> Screen -> Screen 
rotateRowLoop columns x y steps originalScreen currentScreen = 
  if x < columns then 
    let 
      maybeValue = Array2D.get y x originalScreen 
      xRot = (x + steps) |> modBy columns
    in 
      case maybeValue of 
        Just v -> 
          let 
            nextScreen = Array2D.set y xRot v currentScreen 
          in
            rotateRowLoop columns (x + 1) y steps originalScreen nextScreen 
        Nothing ->
          -- Should never happen...
          currentScreen
  else 
    currentScreen

rotateRow : (Int, Int) -> Screen -> Screen 
rotateRow (y, steps) screen = 
  let 
    columns = Array2D.columns screen 
  in 
    rotateRowLoop columns 0 y steps screen screen 

rotateColumnLoop : Int -> Int -> Int -> Int -> Screen -> Screen -> Screen 
rotateColumnLoop rows x y steps originalScreen currentScreen = 
  if y < rows then 
    let 
      maybeValue = Array2D.get y x originalScreen 
      yRot = (y + steps) |> modBy rows
    in 
      case maybeValue of 
        Just v -> 
          let 
            nextScreen = Array2D.set yRot x v currentScreen 
          in
            rotateColumnLoop rows x (y + 1) steps originalScreen nextScreen 
        Nothing ->
          -- Should never happen...
          currentScreen
  else 
    currentScreen

rotateColumn : (Int, Int) -> Screen -> Screen 
rotateColumn (x, steps) screen = 
  let 
    rows = Array2D.rows screen 
  in 
    rotateColumnLoop rows x 0 steps screen screen

execute : Operation -> Screen -> Screen  
execute op screen = 
  case op of 
    Rect (w, h) -> 
      rect (w, h) screen 
    RotateRow (y, steps) -> 
      rotateRow (y, steps) screen
    RotateColumn (x, steps) -> 
      rotateColumn (x, steps) screen

updateModel : Model -> Model
updateModel model = 
  let 
    ops = model.operations
  in 
    case model.operations of 
      [] -> { model | lastCommandText = "done", paused = True, prevOperation = Nothing } 
      op :: rest -> 
        let
          screen = execute op model.screen 
        in 
          { model | screen = screen, prevOperation = Just op, operations = rest, lastCommandText = "updated" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Tick ->
      (updateModel model, Cmd.none)
    Step ->
      (updateModel model, Cmd.none)
    TogglePlay -> 
      let 
        runningText = "running..."
      in 
        ({model | paused = not model.paused, lastCommandText = if model.paused then runningText else "press play to resume" }, Cmd.none)
    Faster -> 
      ({model | delay = model.delay / 2 }, Cmd.none)
    Slower -> 
      ({model | delay = model.delay * 2 }, Cmd.none)
    UseSample -> 
      (updateUseSample model, Cmd.none)
    UseInput -> 
      (updateUseInput model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.delay (\_ -> Tick)
  in 
    Sub.batch [ tickSub ]

-- VIEW

toCharElement : Model -> Pos -> Html Msg 
toCharElement model (x, y) = 
    case Array2D.get y x model.screen of 
      Nothing -> Html.text "?"
      Just lit -> 
        if lit then Html.text "#" else Html.text "."

countLit : Screen -> Int 
countLit screen = 
  let 
    cols = Array2D.columns screen 
    rows = Array2D.rows screen
    posList = getPositions cols rows 
  in 
    posList |> List.filter (\(x, y) -> Array2D.get y x screen |> Maybe.withDefault False) |> List.length 

view : Model -> Html Msg
view model =
  let
    screen = model.screen 
    ys = List.range 0 (Array2D.rows screen - 1)
    xs = List.range 0 (Array2D.columns screen - 1)
    nestedPositions = ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))
    nestedElements = nestedPositions |> List.map (\positions -> positions |> List.map (toCharElement model))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    commandsStr = model.lastCommandText
    litStr = screen |> countLit |> String.fromInt
    opStr = 
      case model.prevOperation of 
        Nothing -> ""
        Just op -> 
          case op of 
            Rect (w, h) -> "rect " ++ String.fromInt w ++ "x" ++ String.fromInt h
            RotateRow (index, steps) -> "rotate row y=" ++ String.fromInt index ++ " by " ++ String.fromInt steps
            RotateColumn (index, steps) -> "rotate column x=" ++ String.fromInt index ++ " by " ++ String.fromInt steps
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
              [ Html.div [] [Html.text "Advent of Code 2016" ]
              , Html.div [] [Html.text "Day 8: Two-Factor Authentication" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2016/day/8" ] 
                [ Html.text "https://adventofcode.com/2016/day/8" ]
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
                [ text "Clear" ]
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
                [ text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text litStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] elements
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text opStr ]
              ] ] ]

