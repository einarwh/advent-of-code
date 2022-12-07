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
delay = 1000

crateSize : Int 
crateSize = 20

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
            , commands = commands
            , keepOrder = False
            , counter = 0
            , debug = "..."}
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick | Step 

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

toCommandText : Command -> String 
toCommandText cmd = 
  "move " ++ String.fromInt cmd.amount ++ " from " ++ String.fromInt cmd.source ++ " to " ++ String.fromInt cmd.target

updateModel : Model -> Model
updateModel model =
  case model.commands of 
    [] -> model 
    cmd :: restCmds -> 
      let 
        updatedStacks = runCommand model.keepOrder cmd model.stacks
        updatedCounter = model.counter + 1 
        debugText = toCommandText cmd
      in 
        { model | counter = updatedCounter, stacks = updatedStacks, commands = restCmds, debug = debugText }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)
    Step ->
      (updateModel model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if List.isEmpty model.commands then Sub.none 
  else Time.every delay (\_ -> Tick)

  -- Time.every delay (\_ -> Tick)

-- VIEW

toCrateRect : Int -> Int -> Int -> Html Msg 
toCrateRect yMax stackId crateNo = 
  let 
    w = crateSize
    h = crateSize
    xVal = (1 + stackId) * 2 * w
    yVal = (1 + crateNo) * h
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt (yMax - yVal))
      , width (String.fromInt w) 
      , height (String.fromInt h)
      , stroke "black"
      , fill "none" ]
      []

toCrateText : Int -> Int -> Int -> Crate -> Html Msg 
toCrateText yMax stackId crateNo crate = 
  let 
    w = crateSize
    h = crateSize
    xOffset = 4
    yOffset = 18
    xVal = xOffset + (1 + stackId) * 2 * w
    yVal = (1 + crateNo) * h - yOffset
  in
    text_
      [ x (String.fromInt xVal)
      , y (String.fromInt (yMax - yVal))
      , fontFamily "monospace" ]
      [ text (String.fromChar crate) ]

toCrateSvgElements : Int -> Int -> Int -> Crate -> List (Html Msg)
toCrateSvgElements yMax stackId crateNo crate = 
  [ toCrateRect yMax stackId crateNo
  , toCrateText yMax stackId crateNo crate ]

toStackSvgElements : Int -> (Int, Stack) -> List (Html Msg)
toStackSvgElements yMax (stackId, stack) =
  let 
    crateElements = 
      stack |> List.indexedMap (toCrateSvgElements yMax stackId)
            |> List.concat
  in 
    crateElements

toSvg : Stacks -> Html Msg 
toSvg stacks = 
  let 
    lst = stacks |> Dict.toList |> List.concatMap (toStackSvgElements 400)
  in 
    svg
      [ viewBox "0 0 400 400"
      , width "400"
      , height "400"
      , Svg.Attributes.style "background-color:lightgrey" ]
      lst

view : Model -> Html Msg
view model =
  let
    s = toSvg model.stacks
    tickStr = "commands: " ++ String.fromInt model.counter
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
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              []
              [ Html.button [ onClick Step ] [ text "Step" ] ] ] ]
