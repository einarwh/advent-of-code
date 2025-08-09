module Aoc21 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html exposing (text)
import Array exposing (Array)
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

type PasswordState = Scrambled | PlainText

type alias Model = 
  { state : PasswordState
  , password : String
  , prevPassword : String 
  , operations : List String
  , descramble : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String
  , debug : String }

inputPlain : String
inputPlain = "abcdefgh"

inputScrambled : String
inputScrambled = "fbgdceah"

input : String 
input = """rotate right 4 steps
swap letter b with letter e
swap position 1 with position 3
reverse positions 0 through 4
rotate left 5 steps
swap position 6 with position 5
move position 3 to position 2
move position 6 to position 5
reverse positions 1 through 4
rotate based on position of letter e
reverse positions 3 through 7
reverse positions 4 through 7
rotate left 1 step
reverse positions 2 through 6
swap position 7 with position 5
swap letter e with letter c
swap letter f with letter d
swap letter a with letter e
swap position 2 with position 7
swap position 1 with position 7
swap position 6 with position 3
swap letter g with letter h
reverse positions 2 through 5
rotate based on position of letter f
rotate left 1 step
rotate right 2 steps
reverse positions 2 through 7
reverse positions 5 through 6
rotate left 6 steps
move position 2 to position 6
rotate based on position of letter a
rotate based on position of letter a
swap letter f with letter a
rotate right 5 steps
reverse positions 0 through 4
swap letter d with letter c
swap position 4 with position 7
swap letter f with letter h
swap letter h with letter a
rotate left 0 steps
rotate based on position of letter e
swap position 5 with position 4
swap letter e with letter h
swap letter h with letter d
rotate right 2 steps
rotate right 3 steps
swap position 1 with position 7
swap letter b with letter e
swap letter b with letter e
rotate based on position of letter e
rotate based on position of letter h
swap letter a with letter h
move position 7 to position 2
rotate left 2 steps
move position 3 to position 2
swap position 4 with position 6
rotate right 7 steps
reverse positions 1 through 4
move position 7 to position 0
move position 2 to position 0
reverse positions 4 through 6
rotate left 3 steps
rotate left 7 steps
move position 2 to position 3
rotate left 6 steps
swap letter a with letter h
rotate based on position of letter f
swap letter f with letter c
swap position 3 with position 0
reverse positions 1 through 3
swap letter h with letter a
swap letter b with letter a
reverse positions 2 through 3
rotate left 5 steps
swap position 7 with position 5
rotate based on position of letter g
rotate based on position of letter h
rotate right 6 steps
swap letter a with letter e
swap letter b with letter g
move position 4 to position 6
move position 6 to position 5
rotate based on position of letter e
reverse positions 2 through 6
swap letter c with letter f
swap letter h with letter g
move position 7 to position 2
reverse positions 1 through 7
reverse positions 1 through 2
rotate right 0 steps
move position 5 to position 6
swap letter f with letter a
move position 3 to position 1
move position 2 to position 4
reverse positions 1 through 2
swap letter g with letter c
rotate based on position of letter f
rotate left 7 steps
rotate based on position of letter e
swap position 6 with position 1"""

initModel : PasswordState -> Bool -> Model 
initModel state descramble = 
  let
    operations = input |> String.split "\n"
    debug = operations |> List.length |> String.fromInt
    password = 
      case state of 
        Scrambled -> inputScrambled
        PlainText -> inputPlain
  in 
    { state = state
    , password = password
    , prevPassword = ""
    , operations = if descramble then List.reverse operations else operations
    , descramble = descramble 
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , message = ""
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel PlainText False, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleDescramble
  | UsePlainText
  | UseScrambled
  | Reset 

updateReset : Model -> Model
updateReset model = 
  initModel model.state model.descramble

swapLetters : String -> String -> String -> String
swapLetters letter1 letter2 password = 
  password |> String.replace letter1 "_" |> String.replace letter2 letter1 |> String.replace "_" letter2

trySwapLetters : String -> String -> String 
trySwapLetters op pwd = 
  case op |> String.split " " of 
    [ _, _, letter1, _, _, letter2 ] -> 
      pwd |> swapLetters letter1 letter2
    _ ->
      pwd

trySwapLettersInverse = trySwapLetters

trySwapPositions : String -> String -> String 
trySwapPositions op pwd =
  case op |> String.split " " of 
    [ _, _, str1, _, _, str2 ] -> 
      case (String.toInt str1, String.toInt str2) of 
        (Just pos1, Just pos2) -> 
          let 
            letter1 = String.slice pos1 (pos1 + 1) pwd 
            letter2 = String.slice pos2 (pos2 + 1) pwd 
          in 
            swapLetters letter1 letter2 pwd 
        _ -> 
          pwd 
    _ ->
      pwd

trySwapPositionsInverse = trySwapPositions

rotateLeft : Int -> String -> String
rotateLeft steps pwd = 
  let 
    left = String.left steps pwd 
    right = String.dropLeft steps pwd 
  in 
    right ++ left

rotateRight : Int -> String -> String
rotateRight steps pwd = 
  let 
    right = String.right steps pwd 
    left = String.dropRight steps pwd 
  in 
    right ++ left

tryRotateLeft : String -> String -> String 
tryRotateLeft op pwd =
  case op |> String.split " " of 
    [ _, _, str, _ ] -> 
      case String.toInt str of 
        Just steps -> 
          rotateLeft steps pwd
        Nothing -> 
          pwd 
    _ ->
      pwd

tryRotateLeftInverse = tryRotateRight

tryRotateRight : String -> String -> String 
tryRotateRight op pwd = 
  case op |> String.split " " of 
    [ _, _, str, _ ] -> 
      case String.toInt str of 
        Just steps -> 
          rotateRight steps pwd 
        Nothing -> 
          pwd 
    _ ->
      pwd

tryRotateRightInverse = tryRotateLeft 

tryRotatePosition : String -> String -> String 
tryRotatePosition op pwd = 
  -- rotate based on position of letter e
  case op |> String.split " " |> List.reverse |> List.head of 
    Just letter -> 
      case pwd |> String.indexes letter |> List.head of 
        Just ix -> 
          let
            steps = if ix >= 4 then ix + 2 else ix + 1 
          in 
            rotateRight steps pwd 
        Nothing -> 
          pwd  
    Nothing ->
      pwd 

tryRotatePositionInverse : String -> String -> String 
tryRotatePositionInverse op pwd = 
  -- rotate based on position of letter e
  case op |> String.split " " |> List.reverse |> List.head of 
    Just letter -> 
      case pwd |> String.indexes letter |> List.head of 
        Just ix -> 
          let
            reversedIx = 
              if ix == 0 then 
                (2 * String.length pwd - 2) // 2
              else if (ix |> modBy 2) == 0 then 
                (ix + String.length pwd - 2) // 2
              else 
                (ix - 1) // 2        
            steps = if reversedIx >= 4 then reversedIx + 2 else reversedIx + 1 
          in 
            rotateLeft steps pwd 
        Nothing -> 
          pwd  
    Nothing ->
      pwd 

tryReversePositions : String -> String -> String 
tryReversePositions op pwd = 
  -- reverse positions 0 through 4
  case op |> String.split " " of 
    [ _, _, str1, _, str2 ] -> 
      case (String.toInt str1, String.toInt str2) of 
        (Just pos1, Just pos2) -> 
          let
            before = String.left pos1 pwd 
            after = String.dropLeft (pos2 + 1) pwd  
            section = pwd |> String.left (pos2 + 1) |> String.dropLeft pos1 
            reversed = section |> String.toList |> List.reverse |> String.fromList 
          in 
            before ++ reversed ++ after
        _ -> 
          pwd 
    _ ->
      pwd

tryReversePositionsInverse = tryReversePositions

movePosition : Int -> Int -> String -> String
movePosition pos1 pos2 pwd = 
  let 
    ch = String.slice pos1 (pos1 + 1) pwd 
    before1 = String.left pos1 pwd 
    after1 = String.dropLeft (pos1 + 1) pwd 
    removed = before1 ++ after1 
    before2 = String.left pos2 removed 
    after2 = String.dropLeft (pos2) removed 
  in 
    before2 ++ ch ++ after2

tryMovePosition : String -> String -> String 
tryMovePosition op pwd =
  -- move position 3 to position 2
  case op |> String.split " " of 
    [ _, _, str1, _, _, str2 ] -> 
      case (String.toInt str1, String.toInt str2) of 
        (Just pos1, Just pos2) -> 
          movePosition pos1 pos2 pwd 
        _ -> 
          pwd 
    _ ->
      pwd

tryMovePositionInverse : String -> String -> String 
tryMovePositionInverse op pwd =
  -- move position 3 to position 2
  case op |> String.split " " of 
    [ _, _, str1, _, _, str2 ] -> 
      case (String.toInt str1, String.toInt str2) of 
        (Just pos1, Just pos2) -> 
          movePosition pos2 pos1 pwd 
        _ -> 
          pwd 
    _ ->
      pwd

scramblePassword op pwd =
  if op |> String.startsWith "swap letter" then 
    trySwapLetters op pwd
  else if op |> String.startsWith "swap position" then 
    trySwapPositions op pwd 
  else if op |> String.startsWith "rotate left" then 
    tryRotateLeft op pwd 
  else if op |> String.startsWith "rotate right" then 
    tryRotateRight op pwd 
  else if op |> String.startsWith "rotate based" then 
    tryRotatePosition op pwd 
  else if op |> String.startsWith "reverse positions" then 
    tryReversePositions op pwd  
  else if op |> String.startsWith "move position" then 
    tryMovePosition op pwd 
  else 
    pwd

descramblePassword op pwd = 
  if op |> String.startsWith "swap letter" then 
    trySwapLettersInverse op pwd
  else if op |> String.startsWith "swap position" then 
    trySwapPositionsInverse op pwd 
  else if op |> String.startsWith "rotate left" then 
    tryRotateLeftInverse op pwd 
  else if op |> String.startsWith "rotate right" then 
    tryRotateRightInverse op pwd 
  else if op |> String.startsWith "rotate based" then 
    tryRotatePositionInverse op pwd 
  else if op |> String.startsWith "reverse positions" then 
    tryReversePositionsInverse op pwd  
  else if op |> String.startsWith "move position" then 
    tryMovePositionInverse op pwd 
  else 
    pwd

updateStep : Model -> Model
updateStep model = 
  case model.operations of 
    [] -> 
      { model | finished = True, paused = True, debug = "" }
    op :: rest ->
      let 
        prevPwd = model.password 
        pwd = if model.descramble then descramblePassword op model.password else scramblePassword op model.password 
      in 
        { model | password = pwd, prevPassword = prevPwd, operations = rest, debug = op }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.state model.descramble
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleDescramble : Model -> Model
updateToggleDescramble model = 
  let
    descramble = not model.descramble
  in
    initModel model.state descramble

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      (updateReset model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    ToggleDescramble -> 
      (updateToggleDescramble model, Cmd.none)
    UsePlainText -> 
      (initModel PlainText False, Cmd.none)
    UseScrambled -> 
      (initModel Scrambled True, Cmd.none)

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
  { title = "Advent of Code 2016 | Day 21: Scrambled Letters and Hash"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    pwdText = 
      if model.finished || String.isEmpty model.prevPassword then 
        model.password 
      else 
        model.prevPassword ++ " -> " ++ model.password
    pwdElement = Html.span [] [ Html.text pwdText ]
    dbgStr = model.debug
    playButtonText = 
      case model.state of 
        PlainText -> "Scramble"
        Scrambled -> "Unscramble"
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
              [ Html.Attributes.align "center" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UsePlainText, Html.Attributes.checked (model.state == PlainText) ] 
                []
              , Html.label [] [ Html.text "Plain" ]
              , 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseScrambled, Html.Attributes.checked (model.state == Scrambled) ] 
                []
              , Html.label [] [ Html.text "Scrambled" ]
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
                [ Html.Attributes.style "width" "100px", onClick TogglePlay ] 
                [ if model.paused then text playButtonText else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleDescramble, Html.Attributes.checked model.descramble ] 
                []
              , Html.label [] [ Html.text " Descramble" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] [ pwdElement ]
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] [ Html.text dbgStr ]
              ] ] 
              ]
