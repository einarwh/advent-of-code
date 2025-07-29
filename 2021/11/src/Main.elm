module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

octopusRadius : Int
octopusRadius = 20

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Tiny | Sample | Input

type alias Grid = 
  { width : Int 
  , height : Int 
  , array : Array Int }

type alias Position = (Int, Int)

type alias Model = 
  { grid : Grid
  , dataSource : DataSource
  , paused : Bool
  , stopAfter100Steps : Bool
  , stopWhenAllFlash : Bool
  , tickInterval : Float
  , steps : Int 
  , total : Int
  , last : Int
  , debug : String }

toEnergyLevel : Char -> Int
toEnergyLevel ch = ch |> String.fromChar |> String.toInt |> Maybe.withDefault 0

lookupEnergyLevel : Position -> Grid -> Maybe Int
lookupEnergyLevel pos grid = 
  case pos of 
    (x, y) ->
      let 
        ix = y * grid.width + x 
      in 
        Array.get ix grid.array

pos2str : ( Int, Int ) -> String
pos2str pos = 
  case pos of 
    (x, y) -> "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"

initModel : Bool -> Bool -> DataSource -> Model
initModel stopAfter100Steps stopWhenAllFlash dataSource =
  let 
    tiny = "11111\n19991\n19191\n19991\n11111"
    sample = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
    input = "1224346384\n5621128587\n6388426546\n1556247756\n1451811573\n1832388122\n2748545647\n2582877432\n3185643871\n2224876627" 
    data = 
      case dataSource of 
        Sample -> sample 
        Input -> input
        Tiny -> tiny
    lines = String.lines data 
    numberLines = 
      lines 
      |> List.map (String.toList)
      |> List.map (List.map toEnergyLevel)
    rowCount = List.length numberLines 
    colCount = 
        case List.head numberLines of 
          Nothing -> 0
          Just h -> List.length h 
    arr = numberLines |> List.concat |> Array.fromList
    grid = { array = arr 
           , width = colCount
           , height = rowCount }

    model = { grid = grid
            , dataSource = dataSource
            , paused = True
            , stopAfter100Steps = stopAfter100Steps
            , stopWhenAllFlash = stopWhenAllFlash
            , tickInterval = 100
            , steps = 0
            , total = 0
            , last = 0
            , debug = "..."}
  in 
    model

init : () -> (Model, Cmd Msg)
init _ = 
  (initModel True True Input, Cmd.none)

-- UPDATE

type Msg = Tick | Step | TogglePlay | Faster | Slower | Clear | UseTiny | UseSample | UseInput | ToggleStopAfter100Steps | ToggleStopWhenAllFlash

incrementArray : Array Int -> Array Int 
incrementArray arr = 
  arr |> Array.map (\energy -> energy + 1)

resetFlashedArray : Array Int -> Array Int 
resetFlashedArray arr = 
  arr |> Array.map (\energy -> if energy > 9 then 0 else energy)

findFlashingIndexes : Array Int -> List Int 
findFlashingIndexes arr =
  arr |> Array.indexedMap (\ix energy -> if energy > 9 then Just ix else Nothing) |> Array.toList |> List.filterMap identity

isValidPosition : Int -> Int -> Position -> Bool 
isValidPosition width height (x, y) = 
  x >= 0 && x < width && y >= 0 && y < height

getCandidateNeighbourPositions : Position -> List Position 
getCandidateNeighbourPositions (x, y) = 
  [ (x-1, y-1)
  , (x, y-1)
  , (x+1, y-1)
  , (x-1, y)
  , (x+1, y)
  , (x-1, y+1)
  , (x, y+1)
  , (x+1, y+1) ]

getValidNeighbourPositions : Int -> Int -> Position -> List Position 
getValidNeighbourPositions width height pos = 
  getCandidateNeighbourPositions pos
  |> List.filter (isValidPosition width height)

incrementNeighbours : List Int -> Array Int -> Array Int
incrementNeighbours neighbours arr = 
  arr |> Array.indexedMap (\ix it -> if List.member ix neighbours then it + 1 else it)

isFlashing : Int -> Bool 
isFlashing energyLevel = 
  energyLevel == 10

getFlashingNeighbours : List Int -> Array Int -> List Int 
getFlashingNeighbours neighbours arr = 
  neighbours |> List.filter (\ix -> Array.get ix arr |> Maybe.withDefault 0 |> isFlashing)  

cascade : Int -> Int -> List Position -> Set Position -> Array Int -> (Array Int, Int)
cascade width height flashing flashed arr = 
  case flashing of 
    [] -> (arr, flashed |> Set.size)
    pos :: rest -> 
      if Set.member pos flashed then 
        cascade width height rest flashed arr
      else 
        -- Increment energy in neighbours and find any new flashing octopi
        let
          neighbourPositions = getValidNeighbourPositions width height pos
          neighbourIndexes = neighbourPositions |> List.map (pos2index width)
          incrementedArr = arr |> incrementNeighbours neighbourIndexes
          flashingNeighbours = getFlashingNeighbours neighbourIndexes incrementedArr
          flashingPositions = flashingNeighbours |> List.map (index2pos width)
          updatedFlashing = flashingPositions ++ flashing |> Set.fromList |> Set.toList
        in
          cascade width height updatedFlashing (Set.insert pos flashed) incrementedArr

updateClear : Model -> Model
updateClear model =
  initModel model.stopAfter100Steps model.stopWhenAllFlash model.dataSource

updateStep : Model -> Model
updateStep model =
  let
    grid = model.grid
    incremented = incrementArray grid.array
    flashingIndexes = findFlashingIndexes incremented
    flashing = flashingIndexes |> List.map (index2pos grid.width)
    (arr, flashCount) = cascade grid.width grid.height flashing Set.empty incremented
    updatedGrid = { grid | array = arr |> resetFlashedArray } 
    steps = model.steps + 1
    --paused = steps >= maxSteps
    pauseAfter100Steps = model.stopAfter100Steps && steps == 100
    pauseWhenAllFlash = model.stopWhenAllFlash && (flashCount == Array.length updatedGrid.array)
    paused = model.paused || pauseAfter100Steps || pauseWhenAllFlash
  in
    { model | grid = updatedGrid, steps = steps, paused = paused, total = model.total + flashCount, last = flashCount, debug = "Flashing: " ++ (flashing |> List.length |> String.fromInt) }

updateTogglePlay : Model -> Model
updateTogglePlay model =
  if model.paused then 
    { model | paused = False } |> updateStep
  else 
    { model | paused = True }

updateDataSource : DataSource -> Model -> Model 
updateDataSource dataSource model = 
  initModel model.stopAfter100Steps model.stopWhenAllFlash dataSource

updateToggleStopAfter100Steps : Model -> Model
updateToggleStopAfter100Steps model = 
  { model | stopAfter100Steps = not model.stopAfter100Steps }

updateToggleStopWhenAllFlash : Model -> Model
updateToggleStopWhenAllFlash model = 
  { model | stopWhenAllFlash = not model.stopWhenAllFlash }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateStep model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    TogglePlay ->
      (updateTogglePlay model, Cmd.none)
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    Clear -> 
        (updateClear model, Cmd.none)
    UseTiny ->
      (updateDataSource Tiny model, Cmd.none)
    UseSample ->
      (updateDataSource Sample model, Cmd.none)
    UseInput ->
      (updateDataSource Input model, Cmd.none)
    ToggleStopAfter100Steps ->
      (updateToggleStopAfter100Steps model, Cmd.none)
    ToggleStopWhenAllFlash ->
      (updateToggleStopWhenAllFlash model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then Sub.none 
  else Time.every model.tickInterval (\_ -> Tick)

-- VIEW
  
index2pos : Int -> Int -> Position 
index2pos width ix = 
  let 
    y = ix // width 
    x = ix |> modBy width
  in 
    (x, y)

pos2index : Int -> Position -> Int 
pos2index width (x, y) = 
  x + width * y
  
toColor : Int -> String 
toColor n = 
  case n of 
    0 -> "#b10000"
    1 -> "#ffcbd1"
    2 -> "#f69697"
    3 -> "#ee6b6e"
    4 -> "#f94449"
    5 -> "#ff2c2c"
    6 -> "#f01e2c"
    7 -> "#de0a26"
    8 -> "#d1001f"
    9 -> "#c30010"
    _ -> "#b10000"

toOctopusNumberElement : Position -> Int -> Svg Msg 
toOctopusNumberElement (xVal, yVal) energy = 
  let 
    color = toColor energy
    xStr = String.fromInt (octopusRadius + xVal * 2 * octopusRadius)
    yStr = String.fromInt (octopusRadius + yVal * 2 * octopusRadius) 
    energyStr = String.fromInt energy
  in 
    Svg.text_ [ x xStr, y yStr ] [ Svg.text energyStr ]

toOctopusElement : Position -> Int -> Svg Msg 
toOctopusElement pos energy = 
  case pos of
    (xVal, yVal) -> 
      let 
        color = toColor energy
        xStr = String.fromInt (octopusRadius + xVal * 2 * octopusRadius)
        yStr = String.fromInt (octopusRadius + yVal * 2 * octopusRadius) 
        radius = if energy == 0 then octopusRadius else octopusRadius // 2
      in 
        circle
          [ cx xStr
          , cy yStr
          , r (String.fromInt radius)
          , fill color
          ]
          []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    grid = model.grid 
    octopusElements = grid.array |> Array.indexedMap (\ix energy -> toOctopusElement (index2pos grid.width ix) energy) |> Array.toList
    w = model.grid.width 
    h = model.grid.height
    svgWidth = String.fromInt (2 * octopusRadius * w)
    svgHeight = String.fromInt (2 * octopusRadius * h)
    viewBoxStr = "0 0 " ++ svgWidth ++ " " ++ svgHeight
  in 
    svg
      [ viewBox viewBoxStr
      , width svgWidth
      , height svgHeight
      ]
      octopusElements

view : Model -> Html Msg
view model =
  let
    s = toSvg model
    totalStr = "Total: " ++ String.fromInt model.total
    lastStr = "Flashing: " ++ String.fromInt model.last
    tickStr = "Steps: " ++ String.fromInt model.steps
  in 
    Html.table 
      [ Html.Attributes.style "width" "900px"
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2021" ]
              , Html.div [] [Html.text "Day 11: Dumbo Octopus" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2021/day/11" ] 
                [ text "https://adventofcode.com/2021/day/11" ]
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
              , Html.input
                [ Html.Attributes.type_ "radio", onClick UseTiny, Html.Attributes.checked (model.dataSource == Tiny) ]
                []
              , Html.label [] [ Html.text "Tiny" ]
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
                [ Html.Attributes.type_ "checkbox", onClick ToggleStopAfter100Steps, Html.Attributes.checked model.stopAfter100Steps ] 
                []
              , Html.label [] [ Html.text " Stop after 100 steps" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleStopWhenAllFlash, Html.Attributes.checked model.stopWhenAllFlash ] 
                []
              , Html.label [] [ Html.text " Stop when all flash" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "10px"] 
              [ Html.div [] [ Html.text tickStr ]
              , Html.div [] [ Html.text lastStr ]
              , Html.div [] [ Html.text totalStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New" ] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              ] ] ]
