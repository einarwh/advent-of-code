module Aoc23 exposing (..)

{- Advent of Code 2020. Day 23: Crab Cups -}

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultTickInterval : Float
defaultTickInterval = 100

-- MAIN

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Model = 
  { links : Dict Int Int
  , current : Int 
  , moveNumber : Int 
  , largeNumbers : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String
  , debug : String }

input : String 
input = "326519478"

    -- let links = 
    --     cups 
    --     |> Array.toList 
    --     |> List.indexed 
    --     |> List.fold (fun ls (ix, c) -> ls |> Map.add c (cups[(ix + 1) % cups.Length])) Map.empty

createLinks : List Int -> Dict Int Int 
createLinks cups = 
  cups 
  |> List.indexedMap (\i c -> ((i + 1) |> modBy (List.length cups), c))
  |> List.map (\(i, c) -> (c, cups |> itemAt i |> Maybe.withDefault 0))
  |> Dict.fromList 

initModel : Bool -> Model 
initModel largeNumbers = 
  let
    cups = input |> String.toList |> List.map (String.fromChar) |> List.filterMap (String.toInt)
    links = createLinks cups
    current = cups |> List.head |> Maybe.withDefault 0 
  in 
    { links = links
    , current = current 
    , moveNumber = 1
    , largeNumbers = largeNumbers 
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
  | Faster 
  | Slower 
  | TogglePlay 
  | ToggleLargeNumbers
  | Reset 

lookup : Int -> Dict Int Int -> Int 
lookup curr links =
  Dict.get curr links |> Maybe.withDefault -1

findDestination : List Int -> Int -> Int -> Int 
findDestination removed maxCup i = 
  if i < 1 then findDestination removed maxCup maxCup 
  else if List.member i removed then findDestination removed maxCup (i - 1)
  else i 

move : Int -> (Dict Int Int, Int) -> (Dict Int Int, Int) 
move maxCup (links, curr) = 
  let 
    cup1 = lookup curr links 
    cup2 = lookup cup1 links 
    cup3 = lookup cup2 links 
    next = lookup cup3 links 
    removed = [cup1, cup2, cup3]
    dest = findDestination removed maxCup (curr - 1) 
    updatedLinks = 
      links 
      |> Dict.insert curr next 
      |> Dict.insert dest cup1 
      |> Dict.insert cup3 (lookup dest links)
  in 
    (updatedLinks, next)

updateReset : Model -> Model
updateReset model = 
  initModel model.largeNumbers

updateStep : Model -> Model
updateStep model = model

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.largeNumbers
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleLargeNumbers : Model -> Model
updateToggleLargeNumbers model = 
  let
    largeNumbers = not model.largeNumbers
  in
    initModel largeNumbers

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
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    ToggleLargeNumbers -> 
      (updateToggleLargeNumbers model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toScale : Float -> String 
toScale v = 160 * v |> String.fromFloat 

toCircleElement : Bool -> Float -> Int -> List (Html Msg)
toCircleElement isCurrent angle cup = 
  let 
    color = if isCurrent then "lightgreen" else "none"
    cxPos = cos (degrees (90 - angle))
    cyPos = -1 * sin (degrees (90 - angle))
    cxStr = 160 * cxPos |> String.fromFloat 
    cyStr = 160 * cyPos |> String.fromFloat
    xStr = 160 * cxPos - 6.2 |> String.fromFloat
    yStr = 160 * cyPos + 6.5 |> String.fromFloat
    cc = circle [ cx cxStr, cy cyStr, r "20", stroke "currentcolor", fill color ] []
    txt = text_ [ x xStr, y yStr, fill "currentcolor" ] [ Html.text (String.fromInt cup) ]
  in 
    [ cc, txt ]

toCupsLoop : List Int -> Int -> Int -> Dict Int Int -> List Int 
toCupsLoop acc curr ix links =
  let 
    next = lookup ix links
  in 
    if next == curr then List.reverse acc 
    else if List.length acc > 9 then List.reverse acc
    else toCupsLoop (next :: acc) curr next links 

toCups : Int -> Dict Int Int -> List Int 
toCups curr links = 
  toCupsLoop [ curr ] curr curr links

recreateCups : Int -> Int -> Dict Int Int -> List Int 
recreateCups moveNumber curr links = 
  let 
    cups = toCups curr links 
    moved = cups 
  in 
    moved 

toSvg : Model -> Html Msg
toSvg model =
  let
    -- x axis: cosine (90 - deg)
    -- y axis: sine (90 - deg)
    angles = [0,1,2,3,4,5,6,7,8]
    cups = recreateCups model.moveNumber model.current model.links 
    circleElements = 
      cups 
      |> List.indexedMap (\i c -> toCircleElement (c == model.current) (toFloat (40 * i)) c)
      |> List.concat
    elements = circleElements
  in
    svg
      [ viewBox "-200 -200 400 400"
      , width "400"
      , height "400"
      , Svg.Attributes.style "font-family:Source Code Pro,monospace"
      ]
      elements

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2020 | Day 23: Crab Cups"
  , body = [ viewBody model ] }

itemAt : Int -> List a -> Maybe a 
itemAt index list = 
  list |> List.drop index |> List.head

viewBody : Model -> Html Msg
viewBody model =
  let
    cups = input |> String.toList |> List.map (String.fromChar) |> List.filterMap (String.toInt)
    cupsStr = "?"
    -- cupsStr = cups |> List.map String.fromInt |> String.concat
    -- dbgStr = 
    --   model.links 
    --   |> Dict.toList 
    --   |> List.map (\(k, v) -> String.fromInt k ++ ":" ++ String.fromInt v)
    --   |> String.join ","
    -- cups = recreateCups model.moveNumber model.current model.links 
    -- next = lookup model.current model.links
    -- dbgStr = cups |> List.map String.fromInt |> String.join "."
    -- dbgStr = "next?" ++ (next |> String.fromInt)
    ix = 9
    index = (ix + 1) |> modBy (List.length cups)
    cup = cups |> List.drop (index - 1) |> List.head |> Maybe.withDefault -1
    dbgStr = "index: " ++ (String.fromInt index) ++ ", cup: " ++ String.fromInt cup

    playButtonText = "Play"
    svgElement = toSvg model 
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
              [ Html.div [] [Html.text "Advent of Code 2020" ]
              , Html.div [] [Html.text "Day 23: Crab Cups" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2020/day/23" ] 
                [ Html.text "https://adventofcode.com/2020/day/23" ]
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
                [ Html.text "Slower" ] 
              , Html.button 
                [ Html.Attributes.style "width" "100px", onClick TogglePlay ] 
                [ if model.paused then Html.text playButtonText else Html.text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ Html.text "Faster" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ] 
            ] ]
      -- , Html.tr 
      --     []
      --     [ Html.td 
      --         [ Html.Attributes.align "center" ]
      --         [ Html.input 
      --           [ Html.Attributes.type_ "checkbox", onClick ToggleLargeNumbers, Html.Attributes.checked model.largeNumbers ] 
      --           []
      --         , Html.label [] [ Html.text " LargeNumbers" ]
      --       ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "20px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] [ svgElement ]
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] [ Html.text cupsStr ]
              , Html.div [] [ Html.text dbgStr ]
              ] ] 
              ]
