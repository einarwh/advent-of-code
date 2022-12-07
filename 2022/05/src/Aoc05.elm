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
  , keepOrder : Bool
  , counter : Int 
  , debug : String }

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = ""
    input = "" 

    stacks = 
        Dict.empty 
        |> Dict.insert 1 ['N','Z']
        |> Dict.insert 2 ['D','C','M']
        |> Dict.insert 3 ['P']

    commands = 
        [ { amount = 1, source = 2, target = 1 }
        , { amount = 3, source = 1, target = 3 } 
        , { amount = 2, source = 2, target = 1 } 
        , { amount = 1, source = 1, target = 2 } ]
    
    model = { stacks = stacks
            , commands = []
            , keepOrder = false
            , counter = 0
            , debug = "..."}
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick

takeAmount : Int -> Crates -> Stack -> (Crates, Stack) 
takeAmount amount taken stack =
    if amount > 0 then 
        case stack of 
            [] -> (taken, stack) 
            top :: rest -> 
                takeAmount (amount - 1) (top :: taken) rest 
    else 
        (taken, stack)

moveAmount : Bool -> Int -> Int -> Int -> Stacks -> Stacks
moveAmount keepOrder amount source target stacks = 
    case takeAmount amount [] (Dict.get source stacks |> Maybe.withDefault []) of 
        (taken, stack) -> 
            stacks 
            |> Dict.update source (Maybe.map (\old -> stack))
            |> Dict.update target (Maybe.map (\old -> (if keepOrder then taken |> List.reverse else taken) ++ old))

runCommand : Bool -> Command -> Stacks -> Stacks 
runCommand keepOrder command stacks = 
    moveAmount keepOrder command.amount command.source command.target stacks

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

