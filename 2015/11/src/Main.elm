module Main exposing (..)

import Browser 
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
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample 

type alias Pos = (Int, Int)

type alias Model = 
  { password : String
  , paused : Bool 
  , tickInterval : Float 
  , message : String
  , debug : String }

input : String
input = "hxbxwxba"

initModel : Model 
initModel = 
  let 
    password = input
  in 
    { password = input
    , paused = True
    , tickInterval = defaultTickInterval
    , message = ""
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | NextValid 
  | TogglePlay 
  | Faster 
  | Slower 
  | Reset 

updateReset : Model -> Model
updateReset model = 
  initModel

nextChar : Char -> Char 
nextChar ch = 
  if ch == 'z' then 'a'
  else 
    ch |> Char.toCode |> \n -> n + 1 |> Char.fromCode

nextIncLoop : Int -> Array Char -> Array Char 
nextIncLoop index chars = 
  if index < 0 then chars 
  else 
    let 
      ch = chars |> Array.get index |> Maybe.withDefault 'a'
    in 
      if ch == 'z' then 
        chars |> Array.set index 'a' |> nextIncLoop (index - 1)
      else 
        chars |> Array.set index (nextChar ch)

nextIncremental : String -> String 
nextIncremental password = 
  let 
    len = String.length password 
  in 
    password |> String.toList |> Array.fromList |> nextIncLoop (len - 1) |> Array.toList |> String.fromList 

indexOfLoop : Int -> a -> List a -> Maybe Int 
indexOfLoop ix element list = 
  case list of 
    [] -> Nothing 
    h :: t -> 
      if h == element then Just ix 
      else 
        indexOfLoop (ix + 1) element t 

indexOf : a -> List a -> Maybe Int
indexOf element list =
  indexOfLoop 0 element list 

nextWithoutLetter : Char -> String-> String 
nextWithoutLetter c s = 
  let 
    chars = s |> String.toList 
  in 
    case indexOf c chars of 
      Just ix -> 
        let 
          before = s |> String.left ix 
          afterLen = String.length s - (ix + 1)
          after = List.repeat afterLen 'a' |> String.fromList 
        in 
          before ++ String.fromChar (nextChar c) ++ after 
      Nothing -> 
        s

nextPassword : String -> String  
nextPassword password =
  password |> nextIncremental |>  nextWithoutLetter 'i' |> nextWithoutLetter 'o' |> nextWithoutLetter 'l'

hasIncreasingSequenceLoop : List Char -> Bool 
hasIncreasingSequenceLoop chars = 
  case chars of 
    a :: b :: c :: rest -> 
      let 
        aVal = a |> Char.toCode 
        bVal = b |> Char.toCode 
        cVal = c |> Char.toCode 
      in 
        if bVal - aVal == 1 && cVal - bVal == 1 then 
          True 
        else 
          hasIncreasingSequenceLoop (b :: c :: rest)
    _ -> False 

hasIncreasingSequence : String -> Bool 
hasIncreasingSequence s = 
  s |> String.toList |> hasIncreasingSequenceLoop

hasOverlappingPairsLoop : Int -> List Char -> Bool 
hasOverlappingPairsLoop count chars = 
  case chars of 
    a :: b :: rest -> 
      if a == b then 
        if count == 1 then True 
        else 
          hasOverlappingPairsLoop (count + 1) rest 
      else 
        hasOverlappingPairsLoop count (b :: rest)
    _ -> False

hasOverlappingPairs : String -> Bool 
hasOverlappingPairs s = 
  s |> String.toList |> hasOverlappingPairsLoop 0

isCompliant : String -> Bool 
isCompliant password = 
  hasIncreasingSequence password && hasOverlappingPairs password

updateStep : Model -> Model
updateStep model = 
  let  
    nextPwd = nextPassword model.password
  in 
    if isCompliant nextPwd then 
      { model | password = nextPwd, paused = True, message = "FOUND" } 
    else 
      { model | password = nextPwd, message = nextPwd }

stepUntilValid : Model -> Model 
stepUntilValid model = 
  let 
    pwd = model.password 
  in 
    if isCompliant pwd then 
      { model | message = "compliant: " ++ pwd }
    else 
      let 
        m = updateStep model 
      in 
        stepUntilValid m

updateNextValid : Model -> Model
updateNextValid model = 
  model |> updateStep |> stepUntilValid

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      (updateReset model, Cmd.none)
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
    NextValid -> 
      (updateNextValid model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

getPwdStyle : Bool -> List (Html.Attribute msg)
getPwdStyle compliant = 
  if compliant then 
    [ Html.Attributes.class "mark-ok adaptive" ]
  else
    [ Html.Attributes.class "mark-err adaptive" ]

view : Model -> Html Msg
view model =
  let
    pwd = model.password
    pwdElement = Html.span (getPwdStyle (isCompliant pwd)) [ Html.text pwd ]
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
              [ Html.div [] [Html.text "Advent of Code 2015" ]
              , Html.div [] [Html.text "Day 11: Corporate Policy" ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2015/day/11" ] 
                [ Html.text "https://adventofcode.com/2015/day/11" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick NextValid ] 
                [ Html.text "Next valid"]
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
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Search" else text "Pause" ] 
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
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                pwdElement
              ] ] 
              ]
