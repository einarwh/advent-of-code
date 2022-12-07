module Aoc05 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

delay : Float
delay = 500

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Crate = Char 

type alias Crates = List Crate 

type alias Stack = List Crate 

type alias StackId = Int 

type alias Stacks = Dict StackId Stack 

type alias Command =
    { amount : Int 
    , source : StackId
    , target : StackId }

type alias Model = 
  { stacks : Stacks
  , commands: List Command
  , counter : Int 
  , debug : String }

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = ""
    input = "" 
    
    model = { stacks = Dict.empty
            , commands = []
            , counter = 0
            , debug = "..."}
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick

updateModel : Model -> Model
updateModel model =
  let
    counter = model.counter + 1
  in
    { model | counter = counter }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every delay (\_ -> Tick)

-- VIEW
  

view : Model -> Html Msg
view model =
  let
    s = text "foo"
    tickStr = "Steps: " ++ String.fromInt model.counter
  in 
    Html.table 
      []
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2022" ]
              , Html.div [] [Html.text "Day 5: Supply Stacks" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text tickStr ]
              , Html.div [] [ Html.text model.debug ]
              ] ] ]

