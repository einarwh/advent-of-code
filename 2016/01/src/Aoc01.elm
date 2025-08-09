module Aoc01 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Html exposing (text)
import Time

defaultTickInterval : Float
defaultTickInterval = 50

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type alias Pos = (Int, Int)

type Dir = N | W | S | E

type Instruction = R Int | L Int 

type Step = TurnLeft | TurnRight | MoveForward

type Visit = Vertical | Horizontal | Both 

type alias Model = 
  { pos : Pos 
  , dir : Dir 
  , twice : Maybe Pos
  , route : List (Pos, Dir)
  -- , segments : List 
  , seen : Set Pos 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

moveForward : Dir -> Pos -> Pos 
moveForward dir (x, y) = 
  case dir of 
    N -> (x, y - 1)
    W -> (x - 1, y)
    S -> (x, y + 1)
    E -> (x + 1, y)

turnRight : Dir -> Dir 
turnRight dir = 
  case dir of 
    N -> E
    E -> S 
    S -> W
    W -> N

turnLeft : Dir -> Dir 
turnLeft dir = 
  case dir of 
    N -> W
    E -> N
    S -> E
    W -> S

initModel : Model 
initModel = 
    { pos = (0, 0)
    , dir = N
    , twice = Nothing
    , route = []
    , seen = Set.empty
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | Faster 
  | Slower 
  | Clear 

getNestedPositions : (Int, Int) -> (Int, Int) -> List (List Pos)
getNestedPositions (xMin, yMin) (xMax, yMax) = 
  let
    ys = List.range yMin (yMax - 1)
    xs = List.range xMin (xMax - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear _ = 
  initModel

updateStep : Model -> Model
updateStep model = model 

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel 
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toCharElement : Model -> Pos -> Html Msg 
toCharElement model (x, y) = 
  if model.pos == (x, y) then 
    let 
      posChar = 
        case model.dir of 
          N -> '^'
          W -> '<'
          S -> 'v'
          E -> '>'
    in 
      (Html.span [Html.Attributes.style "background-color" "#CCCCCC" ] [ Html.text (String.fromChar posChar) ]) 
  else 
    Html.text (String.fromChar '.')

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2016 | Day 1: No Time for a Taxicab"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    -- Grid: (-29, -58) to (168, 158)
    topLeft = (-30, -60)
    botRight = (170, 160)
    nestedPositions = getNestedPositions topLeft botRight
    nestedElements = nestedPositions |> List.map (\ps -> ps |> List.map (toCharElement model))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
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
              [ Html.div [] [Html.text "Advent of Code 2016" ]
              , Html.div [] [Html.text "Day 1: No Time for a Taxicab" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2024/day/6" ] 
                [ Html.text "https://adventofcode.com/2024/day/6" ]
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
                Html.div [] [ Html.text "?" ]
              , Html.div [] [ Html.text "?" ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "4px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] ]
