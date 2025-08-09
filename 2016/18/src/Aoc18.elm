module Aoc18 exposing (..)

{- Advent of Code 2016. Day 18: Like a Rogue. -}

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html exposing (text)
import Dict exposing (Dict)
import Set exposing (Set)
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

type alias Model = 
  { row : String 
  , history : List String 
  , safe : Int 
  , dataSource : DataSource
  , rowCount : Int 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

sample : String
sample = """.^^.^.^^^^"""

input : String
input = """.^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^."""

initRowText : DataSource -> String
initRowText dataSource = 
      case dataSource of 
        Sample -> sample 
        Input -> input 

initModel : DataSource -> Model 
initModel dataSource = 
    { row = initRowText dataSource
    , history = [] 
    , rowCount = 0
    , safe = 0
    , dataSource = dataSource
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
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

ruleList = 
  [ (['.', '.', '.'], '.')
  , (['.', '.', '^'], '^')
  , (['.', '^', '.'], '.')
  , (['.', '^', '^'], '^')
  , (['^', '.', '.'], '^')
  , (['^', '.', '^'], '.')
  , (['^', '^', '.'], '^')
  , (['^', '^', '^'], '.') ]

rules : Dict (List Char) Char
rules = Dict.fromList ruleList

windowed : List a -> List (List a)
windowed lst = 
  case lst of 
    [a, b, c] -> [[a, b, c]]
    a :: b :: c :: rest -> 
      [a, b, c] :: windowed (b :: c :: rest)
    _ -> []

getNextRow : String -> String
getNextRow row =
  let 
    exp = "." ++ row ++ "."
  in 
    exp |> String.toList |> windowed |> List.map (\cs -> Dict.get cs rules |> Maybe.withDefault 'X') |> String.fromList

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource 

updateStep : Model -> Model
updateStep model = 
  let 
    next = getNextRow model.row 
    history = (model.row :: model.history) |> List.take 24
    addedSafe = next |> String.filter (\ch -> ch == '.') |> String.length
    safe = model.safe + addedSafe
    rowCount = model.rowCount + 1
    pause = model.paused || rowCount == 40 || rowCount == 400000
  in 
    { model | row = next, rowCount = rowCount, history = history, safe = safe, paused = pause, debug = String.fromInt (String.length next) }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  initModel dataSource  

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
      (updateDataSource Sample model, Cmd.none)
    UseInput -> 
      (updateDataSource Input model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2016 | Day 18: Like a Rogue"
  , body = [ viewBody model ] }

toStyledCharElement : Char -> Html Msg 
toStyledCharElement symbol = 
  case symbol of 
    '^' -> 
      Html.span [Html.Attributes.class "err adaptive" ] [ Html.text "^" ]
    '.' -> 
      Html.span [Html.Attributes.class "ok adaptive" ] [ Html.text "." ]
    _ -> 
      Html.span [Html.Attributes.class "draw adaptive" ] [ Html.text (String.fromChar symbol) ]

viewBody : Model -> Html Msg
viewBody model =
  let
    textFontSize = 
      case model.dataSource of 
        Sample -> "24px"
        Input -> "12px"
    rows = model.row :: model.history
    nestedElements = rows |> List.map (\r -> r |> String.toList |> List.map toStyledCharElement)
    elements = nestedElements |> List.foldl (\a b -> List.append a (Html.br [] [] :: b)) []
    debugStr = model.row |> String.length |> String.fromInt
    dbg = "?"
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
              , Html.div [] [Html.text "Day 18: Like a Rogue" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2024/day/14" ] 
                [ Html.text "https://adventofcode.com/2024/day/14" ]
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
              , Html.Attributes.style "font-size" "16px" ] 
              [ 
                Html.div [] [ Html.text ("Rows: " ++ String.fromInt model.rowCount) ]
              , Html.div [] [ Html.text ("Safe: " ++ String.fromInt model.safe) ]
              -- , Html.div [] [ Html.text ("Debug: " ++ debugStr) ]
              -- , Html.div [] [ Html.text ("Debug: " ++ dbg) ]
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
