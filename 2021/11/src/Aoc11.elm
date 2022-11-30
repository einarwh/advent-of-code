module Aoc11 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

delay : Float
delay = 5000

octopusRadius : Int
octopusRadius = 20

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Grid = 
  { width : Int 
  , height : Int 
  , array : Array Int }

type alias Position = (Int, Int)

type alias Model = 
  { grid : Grid
  , paused : Bool
  , counter : Int 
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

pos2str pos = 
  case pos of 
    (x, y) -> "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = "11111\n19991\n19191\n19991\n11111"
    input = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

    lines = String.lines input 
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
            , paused = True
            , counter = 0
            , total = 0
            , last = 0
            , debug = "..."}
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick | Step

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
        
updateModel : Model -> Model
updateModel model =
  let
    grid = model.grid
    incremented = incrementArray grid.array
    flashingIndexes = findFlashingIndexes incremented
    flashing = flashingIndexes |> List.map (index2pos grid.width)
    (arr, flashCount) = cascade grid.width grid.height flashing Set.empty incremented
    updatedGrid = { grid | array = arr |> resetFlashedArray } 
    steps = model.counter + 1
  in
    { model | grid = updatedGrid, counter = steps, total = model.total + flashCount, last = flashCount, debug = "Flashing: " ++ (flashing |> List.length |> String.fromInt) }

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
  if model.paused then Sub.none 
  else Time.every delay (\_ -> Tick)

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
    0 -> "#ffdce2"
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
    radius = if energy > 9 then octopusRadius else octopusRadius // 2
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
        radius = if energy > 9 then octopusRadius else octopusRadius // 2
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
    octopusElements = grid.array |> Array.indexedMap (\ix energy -> toOctopusNumberElement (index2pos grid.width ix) energy) |> Array.toList
  in 
    svg
      [ viewBox "0 0 400 400"
      , width "400"
      , height "400"
      ]
      octopusElements

view : Model -> Html Msg
view model =
  let
    s = toSvg model
    totalStr = "Flashed total: " ++ String.fromInt model.total
    lastStr = "Flashed last: " ++ String.fromInt model.last
    tickStr = "Steps: " ++ String.fromInt model.counter
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
              [ Html.div [] [Html.text "Advent of Code 2021" ]
              , Html.div [] [Html.text "Day 11: Dumbo Octopus" ]] ]
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
              , Html.div [] [ Html.text lastStr ]
              , Html.div [] [ Html.text totalStr ]
              , Html.div [] [ Html.text model.debug ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              []
              [ Html.button [ onClick Step ] [ text "Step" ] ] ] ]
