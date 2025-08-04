module Main exposing (..)

import Browser 
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html exposing (text)
import Array exposing (Array)
import MD5
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

hack1Loop : Int -> Int -> Model -> Model 
hack1Loop pos index model = 
  let 
    doorCode = model.doorId ++ String.fromInt index 
    hash = MD5.hex doorCode
  in 
    if String.startsWith "00000" hash then
      let 
        hashChars = hash |> String.toList |> Array.fromList 
        ch = hashChars |> Array.get 5 |> Maybe.withDefault '_'
        p = model.password |> Array.set pos (Just ch)
      in 
        { model | password = p, index = index + 1 }
    else if index < model.index + 10000 then 
      hack1Loop pos (index + 1) model 
    else 
      let 
        hashChars = hash |> String.toList |> Array.fromList 
        ch = hashChars |> Array.get 5 |> Maybe.withDefault '_'
        g = model.guess |> Array.set pos (Just ch)
      in 
        { model | guess = g, index = index + 1 }

hack1 : Model -> Model
hack1 model = 
  let 
    pos = model.password |> Array.toList |> findPos 0
  in 
    hack1Loop pos model.index model 

hack2Loop : Int -> Model -> Model 
hack2Loop index model = 
  let 
    doorCode = model.doorId ++ String.fromInt index 
    hash = MD5.hex doorCode
  in 
    if String.startsWith "00000" hash then
      let 
        hashChars = hash |> String.toList |> Array.fromList 
        pos = hashChars |> Array.get 5 |> Maybe.withDefault 'a' |> String.fromChar |> String.toInt |> Maybe.withDefault -1
        ch = hashChars |> Array.get 6 |> Maybe.withDefault '_'
      in 
        if pos >= 0 && pos < 8 then 
          case model.password |> Array.get pos |> Maybe.withDefault Nothing of 
            Just _ -> { model | index = index + 1 }
            Nothing -> 
              let 
                p = model.password |> Array.set pos (Just ch)
              in 
                { model | password = p, index = index + 1 }
        else 
          let 
            g = model.guess |> Array.set pos (Just ch)
          in 
            { model | guess = g, index = index + 1 }
    else 
      let 
        hashChars = hash |> String.toList |> Array.fromList 
        pos = hashChars |> Array.get 5 |> Maybe.withDefault '0' |> String.fromChar |> String.toInt |> Maybe.withDefault 0
        ch = hashChars |> Array.get pos |> Maybe.withDefault '_'
        g = model.guess |> Array.set pos (Just ch)
        m = { model | guess = g }
      in 
        if index < model.index + 10000 then 
          hack2Loop (index + 1) m
        else 
          { m | index = index + 1 }

hack2 : Model -> Model
hack2 model = 
  hack2Loop model.index model 

updateReset : Model -> Model
updateReset model = 
  initModel model.improvedMechanism

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

view : Model -> Html Msg
view model =
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
              , Html.div [] [Html.text "Day 5: How About a Nice Game of Chess?" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2016/day/5" ] 
                [ Html.text "https://adventofcode.com/2016/day/5" ]
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
