-- Advent of Code 2022. 
-- Day 14: Regolith Reservoir.

module Aoc14 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultDelay : Float
defaultDelay = 1000

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Pos = (Int, Int)

type alias Opacity = Float

type alias Layer = Int

type alias EndlessPos = (Pos, Opacity)

type Path = Endless (List EndlessPos) | Fixed (List Pos)

type alias Model = 
  { rocks : Set Pos
  , bottomRock: Int 
  , sand : Set Pos 
  , sandPath : Path 
  , hasFloor : Bool
  , done : Bool
  , delay : Float
  , paused : Bool
  , debug : String }

toPos : String -> Maybe Pos
toPos s = 
  case String.split "," s of 
    xStr :: yStr :: _ -> 
      case (String.toInt xStr, String.toInt yStr) of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing
    _ -> Nothing

pairwise : List a -> List (a, a) 
pairwise lst = 
  let
    skipped = List.drop 1 lst  
  in
    List.map2 Tuple.pair lst skipped  

toPositions : (Pos, Pos) -> List Pos 
toPositions (pos1, pos2) = 
  let
    (startPos, endPos) = if pos1 < pos2 then (pos1, pos2) else (pos2, pos1)
  in
    case (startPos, endPos) of 
      ((x1, y1), (x2, y2)) -> 
        if x1 == x2 then 
          List.range y1 y2 |> List.map (\y -> (x1, y))
        else 
          List.range x1 x2 |> List.map (\x -> (x, y1))

parseLine : String -> Set Pos 
parseLine s = 
  String.split " -> " s 
  |> List.filterMap toPos 
  |> pairwise 
  |> List.concatMap toPositions
  |> Set.fromList 

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""
    input = """..."""

    lines = sample |> String.split "\n"
    rocks = lines
            |> List.map parseLine
            |> List.map (Set.toList)
            |> List.concat
            |> Set.fromList

    bottomRock = rocks |> Set.toList |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0

    model = { rocks = rocks
            , bottomRock = bottomRock
            , sandPath = Fixed []
            , sand = Set.empty
            , hasFloor = False
            , done = False
            , delay = defaultDelay
            , paused = True
            , debug = ".." }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick | Step | TogglePlay | Faster | Slower 

tryFind : (a -> Bool) -> List a -> Maybe a
tryFind pred lst = 
  case lst of 
    [] -> Nothing 
    h :: t -> 
      if pred h then Just h else tryFind pred t

buildSandPath : Bool -> Int -> Pos -> List Pos -> Set Pos -> Path
buildSandPath hasFloor bottomRock (x, y) path occupied = 
  if y == bottomRock && not hasFloor then 
    let 
      basePath = path |> List.map (\p -> (p, 1.0))
      endless = basePath -- Add trailing.
    in 
      Endless (List.reverse basePath) 
  else 
    let
      candidates = 
        if y == bottomRock + 1 && hasFloor then 
          []
        else 
          [ (x, y + 1), (x - 1, y + 1), (x + 1, y + 1) ]
      maybe = candidates |> tryFind (\c -> not <| Set.member c occupied)
    in 
      case maybe of
        Just nextPos -> 
          buildSandPath hasFloor bottomRock nextPos ((x, y) :: path) occupied 
        Nothing -> 
          let
            finishedPath = (x, y) :: path
          in 
            finishedPath |> List.reverse |> Fixed

findSandPath : Bool -> Int -> Set Pos -> Path 
findSandPath hasFloor bottomRock occupied =  
  let 
    startPos = (500, 0)
  in 
    buildSandPath hasFloor bottomRock startPos [] occupied

updateModel : Model -> Model
updateModel model = 
  case model.sandPath of 
    Endless [] -> 
      { model | debug = "Endless []" }
    Endless (h :: t) -> 
      { model | sandPath = Endless (t ++ [h]), debug = "Endless (h :: t)" }
    Fixed [] -> 
      let 
        occupied = Set.union model.rocks model.sand
        sandPath = findSandPath model.hasFloor model.bottomRock occupied 
      in 
        { model | sandPath = sandPath, debug = "Fixed []" }
    Fixed [ last ] -> 
      let
        sand = Set.insert last model.sand
        sandPath = Fixed []
      in
        { model | sandPath = sandPath, sand = sand, debug = "Fixed [last]" }
    Fixed (_ :: t ) -> 
      let
        sandPath = Fixed t 
      in
        { model | sandPath = sandPath, debug = "Fixed (h :: t)" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (updateModel model, Cmd.none)
    Step ->
      (updateModel model, Cmd.none)
    TogglePlay -> 
      ({model | paused = not model.paused }, Cmd.none)
    Faster -> 
      ({model | delay = model.delay / 2 }, Cmd.none)
    Slower -> 
      ({model | delay = model.delay * 2 }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then Sub.none 
  else Time.every model.delay (\_ -> Tick)

-- VIEW

toOccupiedElement : String -> Float -> Pos -> Html Msg 
toOccupiedElement color elemOpacity (posX, posY) = 
  let 
    w = 1
    h = 1
    xOffset = 0
    yOffset = 0
    xVal = xOffset + posX * w
    yVal = yOffset + posY * h
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt w) 
      , height (String.fromInt h)
      , stroke "none"
      , fill color
      , opacity (String.fromFloat elemOpacity) ]
      []

toRockElement : Pos -> Html Msg 
toRockElement pos = toOccupiedElement "black" 1.0 pos

toSandElement : Pos -> Html Msg 
toSandElement pos = toOccupiedElement "brown" 1.0 pos

toFixedElement : Pos -> Html Msg 
toFixedElement pos = toOccupiedElement "brown" 1.0 pos

toEndlessElement : EndlessPos -> Html Msg 
toEndlessElement (pos, opacity) = toOccupiedElement "red" opacity pos

toGrainElements : Path -> List (Html Msg)
toGrainElements path = 
  case path of 
    Endless [] -> []
    Endless (h :: _) -> [ toEndlessElement h ]
    Fixed [] -> []
    Fixed (h :: _) -> [ toFixedElement h ]

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    rockElements = model.rocks |> Set.toList |> List.map toRockElement
    sandElements = model.sand |> Set.toList |> List.map toSandElement
    grainElements = model.sandPath |> toGrainElements
  in 
    -- x  341 -> 659, y  0 -> 159
    svg
      [ viewBox "300 0 400 160"
      , width "600"
      , height "240" 
      , Svg.Attributes.style "background-color:lightblue" ]
      (rockElements ++ sandElements ++ grainElements)

viewHtml : Model -> Html Msg
viewHtml model =
  let
    s = toSvg model
    debugStr = model.debug
  in 
    Html.table 
      [ Html.Attributes.style "font-family" "Monaco"
      ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Monaco"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2022" ]
              , Html.div [] [Html.text "Day 14: Regolith Reservoir" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Monaco"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text debugStr ]
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ text "Step" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ text "Faster" ] ] ] ] 

view : Model -> Browser.Document Msg
view model = 
  { title = "Advent of Code 2022 - Day 14: Regolith Reservoir"
  , body = [ viewHtml model ] }

