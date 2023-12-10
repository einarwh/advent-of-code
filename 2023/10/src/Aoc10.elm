module Aoc10 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array2D exposing (Array2D)

unitSize : Int 
unitSize = 9

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Model = 
  { lines : List String
  , field : Array2D Char
  , loop : List (Int, Int)
  , debug : String }

findStartPos : Array2D Char -> List (Int, Int) -> Maybe (Int, Int)
findStartPos field indexes =
  case indexes of 
    [] -> Just (7, 7) 
    (x, y) :: rest -> 
      case Array2D.get y x field of 
        Just ch -> 
          if ch == 'S' then Just (x, y) 
          else findStartPos field rest
        Nothing -> Just (x, y)

northPipes = ['S','|', 'L', 'J']
westPipes = ['S','-', 'J', '7']
southPipes = ['S','|', '7', 'F']
eastPipes = ['S','-', 'L', 'F']

checkConnect : Char -> Array2D Char -> (Int, Int) -> List Char -> List Char -> Maybe (Int, Int)
checkConnect pipe field (x, y) sourcePipes targetPipes = 
  if List.member pipe sourcePipes then 
    case Array2D.get y x field of
      Just p -> 
        if List.member p targetPipes then 
          Just (x, y)
        else 
          Nothing 
      Nothing -> Nothing
  else Nothing

findConnections : Array2D Char -> (Int, Int) -> List (Int, Int)
findConnections field (x, y) = 
  case Array2D.get y x field of 
    Just pipe -> 
      [ checkConnect pipe field (x, y - 1) northPipes southPipes 
      , checkConnect pipe field (x - 1, y) westPipes eastPipes 
      , checkConnect pipe field (x, y + 1) southPipes northPipes 
      , checkConnect pipe field (x + 1, y) eastPipes westPipes ]
      |> List.filterMap identity
    Nothing -> []

getIndexes : Array2D Char -> List (Int, Int)
getIndexes field = 
  let 
    width = Array2D.columns field
    height = Array2D.rows field
  in 
    List.range 0 (height - 1) 
    |> List.concatMap (\y -> List.range 0 (width - 1) |> List.map (\x -> (x, y)))

findLoopHelper : Array2D Char -> (Int, Int) -> (Int, Int) -> List (Int, Int) -> List (Int, Int)
findLoopHelper field (x, y) prev positions = 
  case Array2D.get y x field of 
    Just pipe -> 
      if pipe == 'S' then positions 
      else 
        let 
          conns = findConnections field (x, y) 
          filtered = conns |> List.filter (\pos -> pos /= prev)
        in 
          case filtered of 
            [ next ] -> findLoopHelper field next (x, y) ((x, y) :: positions) 
            _ -> [(List.length conns, List.length filtered)]
    Nothing -> [(3, 3)]

findLoop field = 
  let 
    indexes = getIndexes field 
  in 
    case findStartPos field indexes of 
      Just startPos -> 
        case findConnections field startPos of 
          [a, b] -> 
            findLoopHelper field a startPos [startPos]
          _ -> [(1, 1)]
      Nothing -> [(2, 2)]

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."""

    lines = sample |> String.split "\n"

    field = lines |> List.map (String.toList) |> Array2D.fromList

    width = Array2D.columns field
    height = Array2D.rows field
    indexes = 
      List.range 0 (height - 1) 
      |> List.concatMap (\y -> List.range 0 (width - 1) |> List.map (\x -> (x, y)))
    
    (xStart, yStart) = findStartPos field indexes |> Maybe.withDefault (-1, -1)
    startText = "(" ++ String.fromInt xStart ++ ", " ++ String.fromInt yStart ++ ")"

    loop = findLoop field

    foo = indexes |> List.map (\(x, y) -> "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")") |> String.join "-"

    -- debugText = indexes |> List.length |> String.fromInt
    -- debugText = "width (xs) = " ++ String.fromInt width ++ ", height (ys) = " ++ String.fromInt height
    -- debugText = startText
    debugText = "loop length: " ++ (loop |> List.length |> String.fromInt)

    model = { lines = lines 
            , field = field
            , loop = loop
            , debug = debugText }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick 

updateModel : Model -> Model
updateModel model = model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

toCharText : Int -> Int -> Char -> Html Msg 
toCharText yPos xPos ch = 
  let 
    xOffset = 2
    yOffset = 8
    xVal = xOffset + xPos * unitSize
    yVal = yOffset + yPos * unitSize
  in
    text_
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , fontSize "9px"
      , fontFamily "monospace" ]
      [ text (String.fromChar ch) ]

lineToCharTexts : Int -> String -> List (Html Msg)
lineToCharTexts y line =  
    line |> String.toList |> List.indexedMap (toCharText y)

toColoredBox : String -> (Int, Int) -> Html Msg 
toColoredBox fillColor (xStart, yStart) = 
  let 
    xVal = unitSize * xStart
    yVal = unitSize * yStart
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
    --   , stroke "black"
      , opacity "0.6"
      , fill fillColor ]
      []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    lines = model.lines
    loop = model.loop
    loopBoxes = loop |> List.map (toColoredBox "lightgreen")
    charTexts = lines |> List.indexedMap lineToCharTexts |> List.concat
    numberOfLines = lines |> List.length
    numberOfChars = lines |> List.head |> Maybe.map (String.length) |> Maybe.withDefault 0
    svgWidth = (unitSize * numberOfChars) |> String.fromInt
    svgHeight = (unitSize * numberOfLines) |> String.fromInt
    elements = loopBoxes ++ charTexts
  in 
    svg
      [ viewBox ("0 0 " ++ svgWidth ++ svgHeight)
      , width svgWidth
      , height svgHeight ]
      -- , Svg.Attributes.style "background-color:lightblue" ]
      elements

view : Model -> Html Msg
view model =
  let
    s = toSvg model
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
              [ Html.div [] [Html.text "Advent of Code 2023" ]
              , Html.div [] [Html.text "Day 10: Pipe Maze" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text "." ]
              , Html.div [] [ Html.text ".." ]
              , Html.div [] [ Html.text model.debug ]
              ] ] ]
