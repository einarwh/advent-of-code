module Aoc21 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html exposing (text)
import Array exposing (Array)
import Time

defaultTickInterval : Float
defaultTickInterval = 10

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample 

type alias Password = Array (Maybe Char)

type alias Model = 
  { doorId : String 
  , password : Password
  , guess : Password 
  , index : Int
  , improvedMechanism : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String
  , debug : String }

input : String
input = "uqwqemis"

initModel : Bool -> Model 
initModel improvedMechanism = 
  let 
    blank = Array.repeat 8 Nothing 
  in 
    { doorId = input
    , password = blank 
    , guess = blank
    , index = 0
    , improvedMechanism = improvedMechanism 
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , message = ""
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | TogglePlay 
  | ToggleImprovedMechanism
  | Reset 

findPos : Int -> List (Maybe Char) -> Int 
findPos i maybeChars = 
  case maybeChars of 
    [] -> i 
    h :: rest -> 
      case h of 
        Nothing -> i 
        Just _ -> findPos (i + 1) rest 

updateReset : Model -> Model
updateReset model = 
  initModel model.improvedMechanism

hack1 model = model 

hack2 model = model

updateStep : Model -> Model
updateStep model = 
  let 
    m = if model.improvedMechanism then hack2 model else hack1 model 
    count = m.password |> Array.toList |> List.filterMap identity |> List.length 
    finished = count == 8
  in 
    { m | finished = finished, paused = finished }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.improvedMechanism
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleImprovedMechanism : Model -> Model
updateToggleImprovedMechanism model = 
  let
    improvedMechanism = not model.improvedMechanism
  in
    initModel improvedMechanism

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      (updateReset model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    ToggleImprovedMechanism -> 
      (updateToggleImprovedMechanism model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

getChar : Password -> Password -> Int -> Char 
getChar p g index = 
  let 
    pChar = p |> Array.get index |> Maybe.withDefault Nothing 
    gChar = g |> Array.get index |> Maybe.withDefault Nothing 
  in 
    case pChar of 
      Just pc -> pc 
      Nothing -> 
        case gChar of 
          Just gc -> gc 
          Nothing -> '_'

toPasswordString : Model -> String
toPasswordString model = 
  let 
    p = model.password 
    g = model.guess 
  in 
    List.range 0 7 |> List.map (getChar p g) |> String.fromList 

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2016 | Day 21: Scrambled Letters and Hash"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    pwd = toPasswordString model 
    pwdElement = Html.span [] [ Html.text pwd ]
    dbgStr = String.fromInt model.index
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
              , Html.div [] [Html.text "Day 21: Scrambled Letters and Hash" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2016/day/21" ] 
                [ Html.text "https://adventofcode.com/2016/day/21" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Reset ] 
                [ Html.text "Reset"]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Hack door" else text "Pause" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleImprovedMechanism, Html.Attributes.checked model.improvedMechanism ] 
                []
              , Html.label [] [ Html.text " Improved mechanism" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                pwdElement
              -- , Html.div [] [ Html.text dbgStr ]
              ] ] 
              ]
