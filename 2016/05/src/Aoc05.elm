module Aoc05 exposing (..)

import Browser exposing (Document)
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
  , secondDoor : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String
  , debug : String }

input : String
input = "uqwqemis"

initModel : Bool -> Model 
initModel secondDoor = 
  let 
    blank = Array.repeat 8 Nothing 
  in 
    { doorId = input
    , password = blank 
    , guess = blank
    , index = 0
    , secondDoor = secondDoor 
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
  | Step 
  | TogglePlay 
  | Faster 
  | Slower 
  | Reset 

-- let hack1 (doorId : string) = 
--     let rec fn (charsFound : char list) (charsLeft : int) (index : int) (doorId : string) = 
--         if charsLeft > 0 then 
--             let input = doorId + index.ToString()
--             let hash = toHexHash input
--             if hash.StartsWith "00000" then 
--                 let ch = Char.ToLower(hash[5])
--                 printf "%c" ch
--                 fn (ch :: charsFound) (charsLeft - 1) (index + 1) doorId 
--             else 
--                 fn charsFound charsLeft (index + 1) doorId 
--         else 
--             printfn ""
--     fn [] 8 0 doorId

updatePasswords : Bool -> Array Char -> Int -> (Password, Password) -> (Password, Password)
updatePasswords interesting hashChars pos (password, guess) = 
  let 
    ch = Array.get 5 hashChars |> Maybe.withDefault '_'
  in 
    if interesting then 
      (password |> Array.set pos (Just ch), guess)
    else 
      (password, guess |> Array.set pos (Just ch))

findPos : Int -> List (Maybe Char) -> Int 
findPos i maybeChars = 
  case maybeChars of 
    [] -> i 
    h :: rest -> 
      case h of 
        Nothing -> i 
        Just _ -> findPos (i + 1) rest 

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

hack1 model = 
  let 
    pos = model.password |> Array.toList |> findPos 0
  in 
    hack1Loop pos model.index model 

updateReset : Model -> Model
updateReset model = 
  initModel model.secondDoor

updateStep : Model -> Model
updateStep model = 
  hack1 model  

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.secondDoor
    in 
      {m | paused = False }
  else 
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
  { title = "Advent of Code 2016 | Day 5: How About a Nice Game of Chess?"
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
              , Html.div [] [Html.text "Day 5: How About a Nice Game of Chess?" ] ] ]
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
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Find next" else text "Pause" ] 
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
              -- , Html.div [] [ Html.text dbgStr ]
              ] ] 
              ]
