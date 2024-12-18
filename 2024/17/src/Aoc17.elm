module Aoc17 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)
import Html exposing (text)
import Time

-- MAIN

defaultBackgroundColor = "#808080"
lightBackgroundColor = "#989898"
darkBackgroundColor = "#696969"

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type alias Computer = 
  { regA : Int 
  , regB : Int 
  , regC : Int 
  , pointer : Int 
  , program : Array Int 
  , outputs : List Int }

type alias Model = 
  { computer : Computer 
  , dataSource : DataSource
  , backgroundColor : String 
  , debug : String }

-- parseNumbers : String -> List Int 
-- parseNumbers line = 
--   line |> String.split " " |> List.filterMap String.toInt

-- pairwise : List a -> List (a, a) 
-- pairwise lst = 
--   case lst of 
--     x :: y :: rest -> 
--       (x, y) :: pairwise (y :: rest)
--     _ -> []

parseRegister : String -> Int 
parseRegister s = 
  case s |> String.split ": " of 
    [_, str] -> str |> String.toInt |> Maybe.withDefault 0 
    _ -> 0 

parseProgram : String -> Array Int 
parseProgram s = 
  case s |> String.split ": " of 
    [_, str] -> 
      str |> String.split "," |> List.filterMap (String.toInt) |> Array.fromList
    _ -> [0] |> Array.fromList 

parseComputer : String -> Computer 
parseComputer data = 
  case String.lines data of 
    [a, b, c, _, p] -> 
      { regA = parseRegister a 
      , regB = parseRegister b 
      , regC = parseRegister c 
      , pointer = 0
      , program = parseProgram p
      , outputs = [] }
    _ -> 
      { regA = 0
      , regB = 0 
      , regC = 0 
      , pointer = 0 
      , program = Array.empty 
      , outputs = [] } 

initComputer : DataSource -> Computer
initComputer dataSource = 
  let 
    sample = """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""
    sample2 = """Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"""
    input = """Register A: 37283687
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0""" 
    data = 
      case dataSource of 
        Sample -> sample
        Input -> input 
    computer = parseComputer data 
  in 
    computer

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    computer = initComputer dataSource
    model = { computer = computer
            , dataSource = dataSource
            , backgroundColor = ""
            , debug = "" }
  in 
    model 

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Input, Cmd.none)

-- UPDATE

type Msg = 
  Clear 
  | Solve 
  | UseSample 
  | UseInput
  | DefaultBackgroundTick
  | DarkBackgroundTick 
  | LightBackgroundTick 

updateClear : Model -> Model
updateClear model = 
  model

updateSolve : Model -> Model
updateSolve model = 
  model 

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  { model | dataSource = dataSource, computer = initComputer dataSource } 

updateBackgroundColor : String -> Model -> Model
updateBackgroundColor color model = 
  { model | backgroundColor = color } 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Solve -> 
      (updateSolve model, Cmd.none)
    UseSample -> 
      (updateDataSource Sample model, Cmd.none)
    UseInput -> 
      (updateDataSource Input model, Cmd.none)
    DefaultBackgroundTick -> 
      (updateBackgroundColor defaultBackgroundColor model, Cmd.none)
    DarkBackgroundTick -> 
      (updateBackgroundColor darkBackgroundColor model, Cmd.none)
    LightBackgroundTick -> 
      (updateBackgroundColor lightBackgroundColor model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    defaultBackgroundTick = Time.every 277 (\_ -> DefaultBackgroundTick)
    darkBackgroundTick = Time.every 1777 (\_ -> DarkBackgroundTick)
    lightBackgroundTick = Time.every 1977 (\_ -> LightBackgroundTick)
  in
    Sub.batch 
    [ defaultBackgroundTick
    , darkBackgroundTick
    , lightBackgroundTick ]

-- VIEW

toUncheckedHtmlElement : List Int -> List (Html Msg) 
toUncheckedHtmlElement numbers =
  [ numbers |> List.map String.fromInt |> String.join " " |> Html.text, Html.br [] [] ]

toUnsafeHtmlElement : List Int -> List (Html Msg) 
toUnsafeHtmlElement numbers =
  let 
    str = numbers |> List.map String.fromInt |> String.join " "
    textElement = Html.text str 
    spanElement = Html.span [ Html.Attributes.style "background-color" "#FAA0A0" ] [ textElement ]
  in 
    [ spanElement, Html.br [] [] ]

toSafeHtmlElement : List Int -> List (Html Msg) 
toSafeHtmlElement numbers =
  let 
    str = numbers |> List.map String.fromInt |> String.join " "
    textElement = Html.text str 
    spanElement = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElement ]
  in 
    [ spanElement, Html.br [] [] ]

toDampenedHtmlElement : Int -> List Int -> List (Html Msg) 
toDampenedHtmlElement index numbers =
  let 
    before = numbers |> List.take index
    fromIndex = numbers |> List.drop index 
    dropped = fromIndex |> List.take 1 
    after = fromIndex |> List.drop 1 
    strBefore = before |> List.map String.fromInt |> String.join " " 
    textElementBefore = Html.text (String.append strBefore " ") 
    spanElementBefore = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElementBefore ]
    strAfter = after |> List.map String.fromInt |> String.join " "
    textElementAfter = Html.text (String.append " " strAfter) 
    spanElementAfter = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElementAfter ]
    strDropped = dropped |> List.map String.fromInt |> String.join " "
    textElementDropped = Html.text strDropped 
    spanElementDropped = 
      Html.span 
        [ Html.Attributes.style "background-color" "#AFE1AF"
        , Html.Attributes.style "color" "#808080"
        , Html.Attributes.style "text-decoration-line" "line-through"] 
        [ textElementDropped ]
    breakElement = Html.br [] []
  in 
    [ spanElementBefore, spanElementDropped, spanElementAfter, breakElement ]

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    svgWidth = String.fromInt (800)
    svgHeight = String.fromInt (400)
    viewBoxStr = "0 0 " ++ svgWidth ++ " " ++ svgHeight
    backgroundColor = model.backgroundColor
    computer = model.computer
    programStr = computer.program |> Array.toList |> List.map String.fromInt |> String.join ","
    textStroke = "black"
    textFill = "#F28C28"
    pointer = 15
    paddings = 1 + 2 * pointer
    pointerPosition = 70
    pointerPositionStr = String.fromInt pointerPosition
    pointerText = String.padLeft paddings ' ' "^"
    borderElement = rect [ x "0", y "0", width svgWidth, height svgHeight, strokeWidth "4px", stroke "black", fill "none" ] [] 
    regABox = rect [ x "60", y "10", width "720", height "40", strokeWidth "2px", stroke "black", fill "none" ] [] 
    regALabel = text_ [ x "20", y "42", stroke textStroke, fill textFill ] [ Svg.text "A" ]
    regAValue = text_ [ x "70", y "42", stroke textStroke, fill textFill ] [ Svg.text (String.fromInt computer.regA) ]
    regBBox = rect [ x "60", y "60", width "720", height "40", strokeWidth "2px", stroke "black", fill "none" ] [] 
    regBLabel = text_ [ x "20", y "92", stroke textStroke, fill textFill ] [ Svg.text "B" ]
    regBValue = text_ [ x "70", y "92", stroke textStroke, fill textFill ] [ Svg.text (String.fromInt computer.regB) ]
    regCBox = rect [ x "60", y "110", width "720", height "40", strokeWidth "2px", stroke "black", fill "none" ] [] 
    regCLabel = text_ [ x "20", y "142", stroke textStroke, fill textFill ] [ Svg.text "C" ]
    regCValue = text_ [ x "70", y "142, stroke textStroke, fill textFill" ] [ Svg.text (String.fromInt computer.regC) ]
    programBox = rect [ x "60", y "160", width "720", height "80", strokeWidth "2px", stroke "black", fill "none" ] [] 
    programLabel = text_ [ x "20", y "192", stroke textStroke, fill textFill ] [ Svg.text "P" ]
    programValue = text_ [ x "70", y "192", stroke textStroke, fill textFill ] [ Svg.text programStr ]

    midLine = line [ x1 "0", y1 "250", x2 "800", y2 "250", strokeWidth "2px", stroke "black" ] []
    pointerHat = text_ [ x "70", y "232", Svg.Attributes.style "white-space:pre", stroke textStroke, fill textFill ] [ Svg.text pointerText ]
    outputsBox = rect [ x "60", y "250", width "720", height "140", strokeWidth "2px", stroke "black", fill "none" ] [] 
  in 
    svg
      [ viewBox viewBoxStr
      , width svgWidth
      , height svgHeight
      , Svg.Attributes.style ("background-color:" ++ backgroundColor)
      -- , Svg.Attributes.style ("color:" ++ "#F28C28")
      ]
      [ borderElement, regALabel, regAValue, regABox, regBLabel, regBValue, regBBox, regCLabel, regCValue, regCBox, programLabel, programValue, programBox, pointerHat, outputsBox ]

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2024 | Day 17: Chronospatial Computer"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    commandsStr = ""
    textFontSize = 
      case model.dataSource of 
        Sample -> "24px" 
        Input -> "14px" 
    elements = []
    s = toSvg model 
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
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 17: Chronospatial Computer" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2024/day/17" ] 
                [ Html.text "https://adventofcode.com/2024/day/17" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseInput, Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSample, Html.Attributes.checked (model.dataSource == Sample) ] 
                []
              , Html.label [] [ Html.text "Sample" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Solve ] 
                [ Html.text "Solve" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text commandsStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ s ]
              ] ] ]
