module Aoc18 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html exposing (text)
import Array exposing (Array)
import Dict exposing (Dict)
import Queue exposing (Queue)
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

type DataSource = Input | Sample | Bonus 

type alias Name = String

type Value = Reg Name | Num Int 

type Instruction = 
  Snd Name 
  | Rcv Name 
  | Set (Name, Value) 
  | Add (Name, Value) 
  | Mul (Name, Value) 
  | Mod (Name, Value) 
  | Jgz (Value, Value) 

type State = 
  Ready 
  | Running 
  | Waiting 
  | Terminated 

type alias Program = 
  { pid : Int 
  , ptr : Int
  , state : State
  , sent : Int
  , sound : Int
  , registers : Dict String Int 
  , inbox : Queue Int 
  , outbox : Queue Int }

type alias Model = 
  { program0 : Program 
  , program1 : Program 
  , duet : Bool
  , instructions : Array Instruction 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

input : String
input = """set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 680
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"""

orElseWith : (() -> Maybe a) -> Maybe a -> Maybe a 
orElseWith ifNoneThunk maybe = 
  case maybe of 
    Just a -> Just a 
    Nothing -> ifNoneThunk()

parseSnd : List String -> Maybe Instruction
parseSnd parts = 
  case parts of 
    ["snd", r] -> Just (Snd r)
    _ -> Nothing 

parseInstruction : String -> Maybe Instruction
parseInstruction line = 
  line 
  |> String.split " "
  |> parseSnd
  |> orElseWith (\_ -> Nothing)

instructions : Array Instruction
instructions = 
  input |> String.split "\n" |> List.filterMap parseInstruction |> Array.fromList

initDim : DataSource -> (Int, Int)
initDim dataSource = 
  case dataSource of 
    Sample -> (11, 7)
    Input -> (101, 103)
    Bonus -> (101, 103)

initProgram : Int -> Program  
initProgram pid = 
  { pid = pid
  , ptr = 0  
  , state = Ready
  , sent = 0
  , sound = 0
  , registers = Dict.empty |> Dict.insert "p" pid
  , inbox = Queue.empty
  , outbox = Queue.empty }

initModel : Bool -> Model 
initModel duet = 
  let 
    p0 = initProgram 0 
    p1 = initProgram 1 
  in 
    { program0 = initProgram 0  
    , program1 = initProgram 1  
    , duet = duet 
    , instructions = instructions  
    , paused = True 
    , finished = False  
    , tickInterval = defaultTickInterval 
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleDuet
  | Faster 
  | Slower 
  | Clear 

updateClear : Model -> Model
updateClear model = 
  initModel model.duet 

updateStep : Model -> Model
updateStep model = model 

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateToggleDuet : Model -> Model
updateToggleDuet model = 
  let
    duet = not model.duet
  in
    initModel duet 

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
    ToggleDuet -> 
      (updateToggleDuet model, Cmd.none)

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
  { title = "Advent of Code 2017 | Day 18: Duet"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    elements = []
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
              [ Html.div [] [Html.text "Advent of Code 2017" ]
              , Html.div [] [Html.text "Day 18: Duet" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2017/day/18" ] 
                [ Html.text "https://adventofcode.com/2017/day/18" ]
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
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleDuet, Html.Attributes.checked model.duet ] 
                []
              , Html.label [] [ Html.text " Duet" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px" ] 
              [ 
                Html.div [] [ Html.text "?" ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] 
              ]
