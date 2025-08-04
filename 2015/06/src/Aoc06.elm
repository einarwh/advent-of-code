module Aoc06 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
import Html exposing (text)

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

type alias Pos = (Int, Int)

type alias Model = 
  { grid : Array2D Int
  , useBrightness : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , lastCommandText : String
  , counter : Int 
  , debug : String }

initModel : Bool -> Model
initModel useBrightness =
  let 
    model = { grid = Array2D.repeat 500 500 0
            , lastCommandText = "press play to start"
            , paused = True 
            , finished = False
            , tickInterval = defaultTickInterval
            , useBrightness = useBrightness
            , counter = 0
            , debug = "" }
  in 
    model 

init : () -> (Model, Cmd Msg)
init _ =
  let 
    model = initModel False
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleBrightness
  | Faster 
  | Slower 
  | Clear 

updateClear : Model -> Model
updateClear model = initModel model.useBrightness

updateStep : Model -> Model
updateStep model = model

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.useBrightness
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleBrightness : Model -> Model
updateToggleBrightness model = 
  let
    useBrightness = not model.useBrightness
  in
    initModel useBrightness

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
    ToggleBrightness -> 
      (updateToggleBrightness model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2015 | Day 6: Probably a Fire Hazard"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    commandsStr = ""
    elements = []
  in 
    Html.table 
      [ 
        Html.Attributes.style "width" "1080px"
      , Html.Attributes.style "font-family" "Courier New"
      ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2015" ]
              , Html.div [] [Html.text "Day 6: Probably a Fire Hazard" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2015/day/6" ] 
                [ Html.text "https://adventofcode.com/2015/day/6" ]
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
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleBrightness, Html.Attributes.checked model.useBrightness ] 
                []
              , Html.label [] [ Html.text " Use brightness" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px" ] 
              [ 
                Html.div [] [ Html.text commandsStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] ]
